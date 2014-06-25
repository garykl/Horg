module Horg.Output.Dot where

import qualified Data.Text as T
import qualified Data.Set as S

import Horg.Heading

data Dot = Dot {
    node1 :: T.Text,
    node2 :: T.Text,
    kind :: Kind } deriving Show

data Kind = Header2Subheader
          | Tag2Header
          | Header2Content deriving (Show, Eq)

showHeadings :: [Heading] -> T.Text
showHeadings hs = T.concat $ [T.pack "digraph G {\nlayout=dot\nrankdir=LR\n"]
                          ++ map showHeading hs
                          ++ [T.singleton '}']


showHeading :: Heading -> T.Text
showHeading h = T.concat [showNodes (T.pack "[shape=ellipse,fontsize=20,fillcolor=]") $ allTags h,
                          showNodes (T.pack "[shape=box,fontsize=20]") $ allHeaders h,
                          showNodes (T.pack "[shape=note,fontsize=10]") $ allContents h,
                          T.unlines $ map showDot . convertHeading $ h]

showNodes :: T.Text -> [T.Text] -> T.Text
showNodes conf =
    T.unlines . (map (\n -> T.append n conf))

showDot :: Dot -> T.Text
showDot d | kind d == Header2Subheader =
                        showDotWith (T.pack "[color=red];") d
          | kind d == Tag2Header =
                        showDotWith (T.pack "[color=green];") d
          | kind d == Header2Content =
                        showDotWith (T.pack "[color=black];") d
          | otherwise = T.empty

quote :: T.Text -> T.Text
quote t = T.concat [T.singleton '"', t, T.singleton '"']

showDotWith :: T.Text -> Dot -> T.Text
showDotWith t d = T.concat [quote $ node1 d, T.pack "->", quote $ node2 d, t]

convertHeading :: Heading -> [Dot]
convertHeading h = allHeaders2contents h
                ++ allTags2headers h
                ++ allHeader2subheaders h

header2subheaders :: Heading -> [Dot]
header2subheaders h =
    map (\s -> Dot (title h) (title s) Header2Subheader) $ subheadings h

allHeader2subheaders :: Heading -> [Dot]
allHeader2subheaders = concat . collect header2subheaders

tags2header :: Heading -> [Dot]
tags2header h =
    let ts = S.toList $ tags h
    in  map (\t -> Dot t (title h) Tag2Header) ts

allTags2headers :: Heading -> [Dot]
allTags2headers = concat . collect tags2header

header2content :: Heading -> Dot
header2content h = Dot (title h)
                       (dotcontent h) Header2Content

dotcontent :: Heading -> T.Text
dotcontent = T.concat
           . map (\t -> T.append t $ T.pack "\\n")
           . T.lines . content

allHeaders2contents :: Heading -> [Dot]
allHeaders2contents h = filter (\hh -> not $ T.null (node2 hh))
                             $ collect header2content h

allContents :: Heading -> [T.Text]
allContents = map quote . filter (not . T.null) . collect dotcontent

allTags :: Heading -> [T.Text]
allTags = map quote . S.toList . collectTags

allHeaders :: Heading -> [T.Text]
allHeaders = map quote . collect title

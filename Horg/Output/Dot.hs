module Horg.Output.Dot where

import qualified Data.Text as T
import qualified Data.Set as S

import Horg.Heading
import qualified Horg.Output.DotConf as C

data Edge = Edge {
    node1 :: T.Text,
    node2 :: T.Text,
    kind :: Kind } deriving Show

data Kind = Header2Subheader
          | Tag2Header
          | Header2Content deriving (Show, Eq)


showHeadings :: C.Conf -> [Heading] -> T.Text
showHeadings conf hs = T.concat $ [T.pack "digraph G {",
                                   T.pack (show $ C.global conf)]
                          ++ map (showHeading conf) hs
                          ++ [T.singleton '}']


showHeading :: C.Conf -> Heading -> T.Text
showHeading conf h =
    T.concat [showNodes (T.pack . show $ C.tag conf) $ allTags h,
              showNodes (T.pack . show $ C.header conf) $ allHeaders h,
              showNodes (T.pack . show $ C.content conf) $ allContents h,
              T.unlines $ map showEdge . convertHeading $ h]

showNodes :: T.Text -> [T.Text] -> T.Text
showNodes conf =
    T.unlines . (map (\n -> T.append n conf))

showEdge :: Edge -> T.Text
showEdge d | kind d == Header2Subheader =
                        showEdgeWith (T.pack "[color=red];") d
          | kind d == Tag2Header =
                        showEdgeWith (T.pack "[color=green];") d
          | kind d == Header2Content =
                        showEdgeWith (T.pack "[color=black];") d
          | otherwise = T.empty

quote :: T.Text -> T.Text
quote t = T.concat [T.singleton '"', t, T.singleton '"']

showEdgeWith :: T.Text -> Edge -> T.Text
showEdgeWith t d = T.concat [quote $ node1 d, T.pack "->", quote $ node2 d, t]

convertHeading :: Heading -> [Edge]
convertHeading h = allHeaders2contents h
                ++ allTags2headers h
                ++ allHeader2subheaders h

header2subheaders :: Heading -> [Edge]
header2subheaders h =
    map (\s -> Edge (title h) (title s) Header2Subheader) $ subheadings h

allHeader2subheaders :: Heading -> [Edge]
allHeader2subheaders = concat . collect header2subheaders

tags2header :: Heading -> [Edge]
tags2header h =
    let ts = S.toList $ tags h
    in  map (\t -> Edge (title h) t Tag2Header) ts

allTags2headers :: Heading -> [Edge]
allTags2headers = concat . collect tags2header

header2content :: Heading -> Edge
header2content h = Edge (title h)
                       (dotcontent h) Header2Content

dotcontent :: Heading -> T.Text
dotcontent = T.concat
           . map (\t -> T.append t $ T.pack "\\n")
           . T.lines . content

allHeaders2contents :: Heading -> [Edge]
allHeaders2contents h = filter (\hh -> not $ T.null (node2 hh))
                             $ collect header2content h

allContents :: Heading -> [T.Text]
allContents = map quote . filter (not . T.null) . collect dotcontent

allTags :: Heading -> [T.Text]
allTags = map quote . S.toList . collectTags

allHeaders :: Heading -> [T.Text]
allHeaders = map quote . collect title

module HeadingShore where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.List.Split (split, dropBlanks, dropDelims, whenElt)

import Helpers (concatPairs, splitWhile, spacing)
import Headings


parseFile :: String -> [Heading]
parseFile cntnt = map (flip parseHeading $ 1)
                    $ allHeaders (prepareFile cntnt) 1

prepareFile :: String -> [T.Text]
prepareFile = filter (not . T.null) -- empty lines are dangerous
            . map T.strip           -- whitespaces are not part of the content
            . T.lines               -- list are superior to work with
            . T.pack                -- T.Text is more performant

-- | given the contents of a heading of level n as a list of strings,
-- | parse the Heading
parseHeading :: [T.Text] -> Int -> Heading
parseHeading cntnt n =
    let (thisCntnt, otherCntnt) =
            splitWhile
            (\h -> (not (checkIfHeading h (n + 1))))
                cntnt
        thisTitle = head thisCntnt
        withoutTitle = if length thisCntnt == 1 then [] else tail thisCntnt
        thisTags = getTags withoutTitle
        thisContent = getNonMeta withoutTitle
        thisProperties = getProperties withoutTitle
        thisSubheadings = map (flip parseHeading $ n + 1)
                              (allHeaders otherCntnt (n + 1))
    in Heading thisTitle thisTags thisContent thisProperties thisSubheadings

getProperties :: [T.Text] -> M.Map T.Text T.Text
getProperties = propertyText2Property . getPropertyText
    where

    propertyText2Property :: [T.Text] -> M.Map T.Text T.Text
    propertyText2Property [] = M.empty
    propertyText2Property ts =
        let relevant = tail ts
        in  M.fromList $ map parseProperty relevant
        where parseProperty :: T.Text -> (T.Text, T.Text)
              parseProperty t =
                  let (key, valRaw) = T.breakOn (T.singleton ':') $ T.tail t
                      val = T.strip $ T.tail valRaw
                  in  (key, val)

    getPropertyText :: [T.Text] -> [T.Text]
    getPropertyText ts = takeWhile (/= T.pack ":END:")
                       $ dropWhile (/= T.pack ":PROPERTIES:")
                       $ ts

getNonMeta :: [T.Text] -> T.Text
getNonMeta = T.concat . (dropWhile (\c -> ':' == (T.head . T.strip) c))

getTags :: [T.Text] -> S.Set T.Text
getTags ts =
    if null (filterTag ts)
        then S.empty
        else let relevant = T.unpack . head . filterTag $ ts
             in  if relevant == ":PROPERTIES:"
                    then S.empty
                    else S.fromList $ map T.pack
                       $ split (dropBlanks . dropDelims $ whenElt (==':'))
                               relevant

tagCondition :: T.Text -> Bool
tagCondition s | T.null s   = False
               | otherwise  = 
                    let ss = T.strip s
                    in  and [T.head ss == ':', T.last ss == ':']

stripColons :: T.Text -> T.Text
stripColons = T.dropAround (\c -> c == ':')

filterTag :: [T.Text] -> [T.Text]
filterTag = filter tagCondition

unfilterTag :: [T.Text] -> [T.Text]
unfilterTag = filter $ not . tagCondition

showHeading :: Heading -> T.Text
showHeading = T.unlines . showHeadingH

    where

    showHeadingH :: Heading -> [T.Text]
    showHeadingH h = showTitle (title h)
              ++ showTags (tags h)
              ++ showProperties (properties h)
              ++ showContent (content h)
              ++ showSubheadings (subheadings h)

        where

        showTitle :: T.Text -> [T.Text]
        showTitle t = [T.append (T.pack "Title: ") t]

        showTags :: S.Set T.Text -> [T.Text]
        showTags ts = T.pack "Tags:" : S.elems ts

        showContent :: T.Text -> [T.Text]
        showContent c = T.pack "Content:" : T.lines c

        showProperties :: M.Map T.Text T.Text -> [T.Text]
        showProperties m = T.pack "Properties:" :
                            M.elems (M.mapWithKey showProperty m)
            where showProperty :: T.Text -> T.Text -> T.Text
                  showProperty k v = T.concat [k, (T.pack ": "), v]

        showSubheadings :: [Heading] -> [T.Text]
        showSubheadings hs = map (\l -> T.append (spacing 1) l)
                         (foldr (++) []
                         $ map showHeadingH hs)

-- | True if line is a header, False if it is not.
checkIfHeading :: T.Text -> Int -> Bool
checkIfHeading line n | T.length line < n  = False
                   | otherwise =
                       and [(T.replicate n (T.singleton '*') == T.take n line),
                            (line `T.index` n /= '*')]

-- | given some content, represented as a list of strings, return the contents
-- | of all headers of a certain level n.
allHeaders :: [T.Text] -> Int -> [[T.Text]]
allHeaders cntnt n =
    let ls = concatPairs $ tail
                         $ split (whenElt (flip checkIfHeading $ n)) cntnt
    in  ls -- dropWhile (\l -> not (checkIfHeading (head l) n)) ls


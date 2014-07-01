module Horg.Parse where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.List.Split (split, dropBlanks, dropDelims, whenElt)
import Data.Time.LocalTime (LocalTime)

import Helpers (concatPairs, splitWhile, stripColons, justFilter)
import Horg.Datetime (parseDateRange, parseDate)
import Horg.Heading


parseFile :: String -> [Heading]
parseFile cntnt =

    let finecontent = prepareFile cntnt
        headings = map (flip parseHeading $ 1)
                     $ allHeaders finecontent 1
        filetags = getFiletags finecontent

    in  map (addTags filetags) headings


prepareFile :: String -> [T.Text]
prepareFile = filter (not . T.null) -- empty lines are dangerous
            . map T.strip           -- whitespaces are not part of the content
            . T.lines               -- list are superior to work with
            . T.pack                -- T.Text is more performant


getFiletags :: [T.Text] -> S.Set T.Text
getFiletags ts =

    let filetagcriterion = \line -> (T.pack "#+FILETAGS:") `T.isPrefixOf` line
        relevant = takeWhile filetagcriterion ts

    in  if null relevant
            then S.empty
            else getTags [T.tail
                        . snd
                        . (T.breakOn (T.singleton ' '))
                        $ head relevant]


-- | given the contents of a heading of level n as a list of strings,
-- | parse the Heading
parseHeading :: [T.Text] -> Int -> Heading
parseHeading cntnt n =

    let (thisCntnt, otherCntnt) =
            splitWhile
            (\h -> (not (checkIfHeading h (n + 1))))
                cntnt
        withoutTitle = if length thisCntnt <= 1 then [] else tail thisCntnt

        (thisState, thisTitle, thisTagsHeader) = getStateTitleTags thisCntnt
        thisTagsRest    = getTags withoutTitle

        thisContent     = getNonMeta withoutTitle
        thisDates       = getDates withoutTitle
        thisLogbook     = getLogbook withoutTitle
        thisProperties  = getProperties withoutTitle
        thisSubheadings = map (flip parseHeading $ n + 1)
                              (allHeaders otherCntnt (n + 1))

    in emptyHeading { title       = thisTitle,
                      tags        = S.union thisTagsHeader thisTagsRest,
                      content     = thisContent,
                      state       = thisState,
                      properties  = thisProperties,
                      logbook     = thisLogbook,
                      dates       = thisDates,
                      subheadings = thisSubheadings }


getDates :: [T.Text] -> [Datetype LocalTime]
getDates [] = []
getDates s =
    let closed = filter (T.isPrefixOf $ T.pack "CLOSED:") s
        scheduled = filter (T.isPrefixOf $ T.pack "SCHEDULED:") s
        deadline = filter (T.isPrefixOf $ T.pack "DEADLINE:") s
    in  filtern Closed (map extractDate closed)
     ++ filtern Scheduled (map extractDate scheduled)
     ++ filtern Deadline (map extractDate deadline)
    where
    extractDate :: T.Text -> Maybe LocalTime
    extractDate = parseDate . unwords . map T.unpack . tail . T.words
    filtern :: (LocalTime -> Datetype LocalTime)
            -> [Maybe LocalTime]
            -> [Datetype LocalTime]
    filtern f ll = map f $ justFilter ll


getStateTitleTags :: [T.Text] -> (Maybe T.Text, T.Text, S.Set T.Text)
getStateTitleTags ts =

    let raw = T.dropWhile (\c -> or [(c == ' '),
                                     (c == '*')])
                        $ head ts
        firstWord = T.takeWhile (/= ' ') raw

    in  if T.length firstWord == T.length raw
            then let (ti, ta) = checkoutTags raw
                 in  (Nothing, ti, ta)
            else
                if isUpper firstWord
                  then let (ti, ta) = checkoutTags
                                    . T.tail $ T.dropWhile (/= ' ') raw
                       in  (Just firstWord, ti, ta)
                  else let (ti, ta) = checkoutTags raw
                       in  (Nothing, ti, ta)

    where

        isUpper :: T.Text -> Bool
        isUpper t = t == T.toUpper t

        checkoutTags :: T.Text -> (T.Text, S.Set T.Text)
        checkoutTags titl =

            let potentialTags = T.dropWhile (/= ':') titl
                potentialTitl = T.takeWhile (/= ':') titl

            in  if tagCondition potentialTags
                    then (T.strip potentialTitl, getTags [potentialTags])
                    else (titl, S.empty)


getLogbook :: [T.Text] -> [(LocalTime, LocalTime)]
getLogbook = logbookText2Logbook . getLogbookText
    where

    logbookText2Logbook :: [T.Text] -> [(LocalTime, LocalTime)]
    logbookText2Logbook [] = []
    logbookText2Logbook ts =

        let relevant = tail ts
        in  map parseLogbook relevant

        where parseLogbook :: T.Text -> (LocalTime, LocalTime)
              parseLogbook t =
                  let relevantH = tail . T.words $ t
                      relevant = take ((length relevantH) - 2) relevantH
                      Just dtr = parseDateRange . T.unpack . T.unwords $ relevant
                  in  dtr

    getLogbookText :: [T.Text] -> [T.Text]
    getLogbookText ts = takeWhile (/= T.pack ":END:")
                  $ dropWhile (/= T.pack ":LOGBOOK:")
                  $ ts

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
getNonMeta = T.unlines
           . dropWhile (T.isPrefixOf (T.pack "CLOSED"))
           . dropWhile (T.isPrefixOf (T.pack "DEADLINE"))
           . dropWhile (T.isPrefixOf (T.pack "SCHEDULED"))
           . dropWhile (\c -> ':' == (T.head . T.strip) c)


getTags :: [T.Text] -> S.Set T.Text
getTags ts =

    if null (filterTag ts)
        then S.empty

        else let relevant = T.unpack . head . filterTag $ ts

             in  if or [relevant == ":PROPERTIES:",
                        relevant == ":LOGBOOK:"]
                    then S.empty

                    else S.fromList $ map T.pack
                       $ split (dropBlanks . dropDelims $ whenElt (==':'))
                               relevant


tagCondition :: T.Text -> Bool
tagCondition s

    | T.null s   = False

    | otherwise  = 
         let ss = T.strip s
         in  and [T.head ss == ':', T.last ss == ':']


filterTag :: [T.Text] -> [T.Text]
filterTag = filter tagCondition


unfilterTag :: [T.Text] -> [T.Text]
unfilterTag = filter $ not . tagCondition


-- | True if line is a header, False if it is not.
checkIfHeading :: T.Text -> Int -> Bool
checkIfHeading line n

    | T.length line < n  = False

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


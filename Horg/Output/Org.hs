module Horg.Output.Org (showHeading) where

import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Time.LocalTime (LocalTime)

import Horg.Heading
import qualified Horg.Datetime as DT
import Helpers (spacing)


showHeading :: Heading -> T.Text
showHeading = T.unlines . (showHeadingH 1)


showHeadingH :: Int -> Heading -> [T.Text]
showHeadingH n h = [T.concat [showTitleAndState n (state h) (title h),
                              T.singleton ' ',
                              showTags n (tags h)]]
                ++ showDates n (dates h)
                ++ showLogbook n (logbook h)
                ++ showProperties n (properties h)
                ++ showContent n (content h)
                ++ showSubheadings n (subheadings h)


showTitleAndState :: Int -> Maybe T.Text -> T.Text -> T.Text
showTitleAndState n s t =
    let tit = case s of
                Nothing -> T.singleton ' '
                Just h -> T.concat [T.singleton ' ', h, T.singleton ' ']
    in  T.concat [T.replicate n (T.singleton '*'), tit, t]


showDates :: Int -> [Datetype LocalTime] -> [T.Text]
showDates n = map (T.append (spacing (n + 1)) . T.pack . show)


showLogbook :: Int -> [(LocalTime, LocalTime)] -> [T.Text]
showLogbook n = map (T.append (spacing (n + 1)) . T.pack . DT.showRange)


showTags :: Int -> S.Set T.Text -> T.Text
showTags n ts | S.null ts   = T.empty
              | otherwise   = T.append (spacing (n + 1)) (showTagsH ts)
    where
    showTagsH t | S.null t    = T.singleton ':'
                | otherwise   = T.concat [T.singleton ':',
                                          S.findMin t,
                                          showTagsH (S.deleteMin t)]

showContent :: Int -> T.Text -> [T.Text]
showContent n c = map (T.append (spacing (n + 1))) $ T.lines c

showProperties :: Int -> M.Map T.Text T.Text -> [T.Text]
showProperties n m | M.null m   = []
                   | otherwise  =
   map (T.append (spacing (n + 1))) $
       (T.pack ":PROPERTIES:" : M.elems (M.mapWithKey showProperty m))
    ++ [T.pack ":END:"]
    where showProperty :: T.Text -> T.Text -> T.Text
          showProperty k v = T.concat [T.singleton ':', k, (T.pack ": "), v]

showSubheadings :: Int -> [Heading] -> [T.Text]
showSubheadings n hs = (foldr (++) [] $ map (showHeadingH (n + 1)) hs)

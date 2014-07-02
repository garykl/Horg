{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Horg.Heading where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.LocalTime (LocalTime)

import qualified Horg.Datetime as DT

-- | an Ord-mode file is a tree of heading with some content
data Heading = Heading {
    title :: T.Text,
    tags :: S.Set T.Text,
    state :: Maybe T.Text,
    content :: T.Text,
    properties :: M.Map T.Text T.Text,
    logbook :: [(LocalTime, LocalTime)],
    dates :: [Datetype LocalTime],
    subheadings :: [Heading] }

data Datetype a where
    Closed :: a -> Datetype a
    Scheduled :: a -> Datetype a
    Deadline :: a -> Datetype a
    Timestamp :: a -> Datetype a

instance Show (Datetype LocalTime) where
    show (Closed d) = "CLOSED: " ++ DT.showLocalTime d
    show (Scheduled d) = "SCHEDULED: " ++ DT.showLocalTime d
    show (Deadline d) = "DEADLINE: " ++ DT.showLocalTime d
    show (Timestamp d) = DT.showLocalTime d

freedate :: Datetype LocalTime -> LocalTime
freedate (Closed d) = d
freedate (Scheduled d) = d
freedate (Deadline d) = d
freedate (Timestamp d) = d

filterclosed :: [Datetype LocalTime] -> Maybe LocalTime
filterclosed [] = Nothing
filterclosed (Closed d:_) = Just d
filterclosed dd = filterscheduled $ tail dd

closed :: Heading -> Maybe LocalTime
closed = filterclosed . dates

filterdeadline :: [Datetype LocalTime] -> Maybe LocalTime
filterdeadline [] = Nothing
filterdeadline (Deadline d:_) = Just d
filterdeadline dd = filterscheduled $ tail dd

deadline :: Heading -> Maybe LocalTime
deadline = filterclosed . dates

filterscheduled :: [Datetype LocalTime] -> Maybe LocalTime
filterscheduled [] = Nothing
filterscheduled (Scheduled d:_) = Just d
filterscheduled dd = filterscheduled $ tail dd

scheduled :: Heading -> Maybe LocalTime
scheduled = filterclosed . dates

emptyHeading :: Heading
emptyHeading = Heading T.empty S.empty Nothing T.empty M.empty [] [] []

addTag :: T.Text -> Heading -> Heading
addTag t h = h { tags = S.insert t $ tags h }

addTags :: S.Set T.Text -> Heading -> Heading
addTags ts h | S.null ts   = h
             | otherwise   = addTags (S.deleteMin ts) (addTag (S.findMin ts) h)


collect :: (Heading -> a) -> Heading -> [a]
collect c h = c h : concatMap (collect c) (subheadings h)

collectTags :: Heading -> S.Set T.Text
collectTags h = S.unions $ collect tags h

-- | traverse a heading, apply a function f to it and all its subheadings.
-- | the provided function should not change the subheadings of a heading.
-- | The resulting behavior would be much to complex to understand.
traverse :: (Heading -> Heading) -> Heading -> Heading
traverse f h =
    let h2 = f h
    in  h2 { subheadings = map f $ subheadings h2 }

tagTraverse :: T.Text -> Heading -> Heading
tagTraverse = traverse . addTag

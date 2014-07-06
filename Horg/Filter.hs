module Horg.Filter where

import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Time.LocalTime (LocalTime)

import qualified Horg.Heading as Heading
import Helpers (justFilter)
-- import qualified Horg.Datetime as DT


data Filter = Filter (Heading.Heading -> Bool)


checkHeading :: Filter -> Heading.Heading -> Bool
checkHeading (Filter f) = f


(*&), (*|) :: Filter -> Filter -> Filter
f *& g = Filter $ \h -> checkHeading f h && checkHeading g h
f *| g = Filter $ \h -> checkHeading f h || checkHeading g h


-- | apply a filter on a heading. when a heading or subheading is not filtered
-- | out, this complete (sub)heading with all its subheadings is returned.
surface:: Filter -> Heading.Heading -> [Heading.Heading]
surface f h =
      if checkHeading f h
          then [h]
          else map (Heading.traverse . Heading.addTags $ Heading.tags h)
             . concatMap (surface f) $ Heading.subheadings h


-- | apply a filter on a heading, when a heading is not filtered out, only its
-- | content is returned and all its children are tested for the criterion
-- | as well.
deep :: Filter -> Heading.Heading -> [Heading.Heading]
deep f h =
      let acc = concatMap (deep f) (Heading.subheadings h)
      in if checkHeading f h
            then [h { Heading.subheadings = acc }]
            else map (Heading.traverse . Heading.addTags $ Heading.tags h) acc


-- | apply a filter on a heading, when a heading matches in an arbitrary
-- | subheading, it is returned.
conserve :: Filter -> Heading.Heading -> [Heading.Heading]
conserve f h = [h | not . null $ surface f h]


preserve :: Filter -> Heading.Heading -> [Heading.Heading]
preserve f h = justFilter [preserve' h]
  where preserve' :: Heading.Heading -> Maybe Heading.Heading
        preserve' hh = 
          let sh = justFilter $ map preserve' $ Heading.subheadings hh
          in  if null sh
                then if checkHeading f hh then Just hh
                             else Nothing
                else Just $ hh { Heading.subheadings = sh }

idAnd :: Filter
idAnd = Filter . const $ True

idOr :: Filter
idOr = Filter . const $ False

tag :: T.Text -> Filter
tag = Filter . hasTag

    where hasTag :: T.Text -> Heading.Heading -> Bool 
          hasTag t h = t `S.member` Heading.tags h


property :: T.Text -> T.Text -> Filter
property a = Filter . hasProperty a

    where hasProperty :: T.Text -> T.Text -> Heading.Heading -> Bool
          hasProperty k v h = k `M.member` Heading.properties h
                           && Heading.properties h M.! k == v


state :: T.Text -> Filter
state = Filter . hasState

    where hasState :: T.Text -> Heading.Heading -> Bool
          hasState s h = case Heading.state h of
                             Nothing -> False
                             Just t  -> t == s


isInfix :: (Heading.Heading -> T.Text) -> T.Text -> Filter
isInfix f t = Filter $ T.isInfixOf t . f


content :: T.Text -> Filter
content = isInfix Heading.content


title :: T.Text -> Filter
title = isInfix Heading.title


timed :: (LocalTime -> LocalTime -> Bool)
      -> Heading.Datetype LocalTime
      -> Filter
timed op dt =
    let kindfunc = case dt of
            Heading.Scheduled _ -> Heading.scheduled
            Heading.Closed _ -> Heading.closed
            Heading.Deadline _ -> Heading.deadline
            _ -> undefined
    in  Filter $ maybe False (flip op $ Heading.freedate dt) . kindfunc


latertimed :: Heading.Datetype LocalTime -> Filter
latertimed = timed (>)

earliertimed :: Heading.Datetype LocalTime -> Filter
earliertimed = timed (<)

equaltimed :: Heading.Datetype LocalTime -> Filter
equaltimed = timed (==)

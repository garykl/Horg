module Horg.Filter where

import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M
import Horg.Heading

data Filter = Filter (Heading -> Bool)

checkHeading :: Filter -> Heading -> Bool
checkHeading (Filter f) = f

(*&), (*|) :: Filter -> Filter -> Filter
f *& g = Filter $ \h -> checkHeading f h && checkHeading g h
f *| g = Filter $ \h -> checkHeading f h || checkHeading g h

-- | apply a filter on a heading. when a heading is not filtered out, the
-- | complete tree and all its subtrees are returned.
surface:: Filter -> Heading -> [Heading]
surface f h =
      if checkHeading f h
          then [h]
          else foldr (++) [] $ map (surface f) (subheadings h)

-- | apply a filter on a heading, when a heading is not filtered out, only its
-- | content is returned and all its children are tested for the criterion
-- | as well.
deep :: Filter -> Heading -> [Heading]
deep f h =
      let acc = foldr (++) [] $ map (deep f) (subheadings h)
      in if checkHeading f h
            then [h { subheadings = acc }]
            else acc

tagFilter :: T.Text -> Filter
tagFilter = Filter . hasTag
    where hasTag :: T.Text -> Heading -> Bool 
          hasTag t h = t `S.member` tags h

propertyFilter :: T.Text -> T.Text -> Filter
propertyFilter a = Filter . (hasProperty a)
    where hasProperty :: T.Text -> T.Text -> Heading -> Bool
          hasProperty k v h =
              if k `M.member` properties h
                   then properties h M.! k == v
                   else False

stateFilter :: T.Text -> Filter
stateFilter = Filter . hasState
    where hasState :: T.Text -> Heading -> Bool
          hasState s h = case state h of
                             Nothing -> False
                             Just t  -> t == s

contentFilter :: T.Text -> Filter
contentFilter = undefined

titleFilter :: T.Text -> Filter
titleFilter = undefined

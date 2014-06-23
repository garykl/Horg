module Horg.Filter where

import Horg.Heading

data Filter = Filter (Heading -> Bool)

checkHeading :: Filter -> Heading -> Bool
checkHeading (Filter f) = f

-- | apply a filter on a heading. when a heading is not filtered out, the
-- | complete tree and all its subtrees are returned.
surfaceFilterHeading :: Filter -> Heading -> [Heading]
surfaceFilterHeading f h =
      if checkHeading f h
          then [h]
          else foldr (++) [] $ map (surfaceFilterHeading f) (subheadings h)

-- | apply a filter on a heading, when a heading is not filtered out, only its
-- | content is returned and all its children are tested for the criterion
-- | as well.
deepFilterHeading :: Filter -> Heading -> [Heading]
deepFilterHeading f h =
      let acc = foldr (++) [] $ map (surfaceFilterHeading f) (subheadings h)
      in if checkHeading f h
            then [h { subheadings = acc }]
            else acc


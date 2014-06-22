module Headings where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

-- | an Ord-mode file is a tree of heading with some content
data Heading = Heading {
    title :: T.Text,
    tags :: S.Set T.Text,
    content :: T.Text,
    properties :: M.Map T.Text T.Text,
    subheadings :: [Heading] }


emptyHeading :: Heading
emptyHeading = Heading T.empty S.empty T.empty M.empty []

addTag :: T.Text -> Heading -> Heading
addTag t h = h { tags = S.insert t $ tags h }

removeTag :: T.Text -> Heading -> Heading
removeTag t h = h { tags = S.delete t $ tags h }

setProperty :: T.Text -> T.Text -> Heading -> Heading
setProperty k v h = h { properties = M.insert k v (properties h) }

removeProperty :: T.Text -> Heading -> Heading
removeProperty k h = h { properties = M.delete k (properties h) }

addSubheading :: Heading -> Heading -> Heading
addSubheading s h = h { subheadings = s : subheadings h }

-- | traverse a heading, apply a function f to it and all its subheadings.
-- | the provided function should not change the subheadings of a heading.
-- | The resulting behavior would be much to complex to understand.
traverse :: (Heading -> Heading) -> Heading -> Heading
traverse f h =
    let h2 = f h
    in  h2 { subheadings = map f $ subheadings h2 }

tagTraverse :: T.Text -> Heading -> Heading
tagTraverse = traverse . addTag

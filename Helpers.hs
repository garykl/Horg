module Helpers where

import qualified Data.Text as T


splitWhile :: (a -> Bool) -> [a] -> ([a], [a])
splitWhile f ll =
    let a = takeWhile f ll
        b = drop (length a) ll
    in  (a, b)


spacing :: Int -> T.Text
spacing n = T.replicate n $ T.singleton ' '


concatPairs :: [[a]] -> [[a]]
concatPairs [] = []
concatPairs [_] = []
concatPairs (l1:l2:ll) = (l1 ++ l2) : concatPairs ll


stripColons :: T.Text -> T.Text
stripColons = T.dropAround (\c -> c == ':')


justFilter :: Eq a => [Maybe a] -> [a]
justFilter = map (\(Just h) -> h) . filter (not . (== Nothing))


(|>>) :: (a -> Maybe b) -> (a -> Maybe b) -> (a -> Maybe b)
f |>> g =
    \a -> case f a of
              Nothing -> g a
              Just r -> Just r

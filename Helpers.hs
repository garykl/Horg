module Helpers where

import qualified Data.Text as T

splitWhile :: (a -> Bool) -> [a] -> ([a], [a])
splitWhile f ll = let a = takeWhile f ll
                      b = drop (length a) ll
                  in  (a, b)

spacing :: Int -> T.Text
spacing n = T.replicate n $ T.singleton ' '

concatPairs :: [[a]] -> [[a]]
concatPairs [] = []
concatPairs [_] = []
concatPairs (l1:l2:ll) = (l1 ++ l2) : concatPairs ll

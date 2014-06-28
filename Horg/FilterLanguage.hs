{-# LANGUAGE GADTs #-}
module Horg.FilterLanguage where

import qualified Data.Text as T
import Data.List

import qualified Horg.Filter as F


data Fexpr a where
    Pack :: F.Filter -> Fexpr F.Filter
    Tag :: String -> Fexpr F.Filter
    Notag :: String -> Fexpr F.Filter
    Content :: String -> Fexpr F.Filter
    Title :: String -> Fexpr F.Filter
    State :: String -> Fexpr F.Filter
    And :: Fexpr a -> Fexpr a -> Fexpr a
    Or :: Fexpr a -> Fexpr a -> Fexpr a


feval :: Fexpr F.Filter -> F.Filter
feval (Pack f) = f
feval (Tag t) = F.tag $ T.pack t
feval (Notag t) = F.tag $ T.pack t
feval (Content c) = F.content $ T.pack c
feval (Title t) = F.title $ T.pack t
feval (State s) = F.state $ T.pack s
feval (And e1 e2) = feval e1 F.*& feval e2
feval (Or e1 e2) = feval e1 F.*| feval e2


parse :: String -> Fexpr F.Filter
parse t
  | not (isWithORs t || isWithParens t) =
    foldl (And) (Pack F.idAnd) $ map parseItem (quotedwords t)
  | otherwise =
      let ors = map extractLeftOuterParen $ extractOuterORs t
          ani = map (\(l, c, r) -> And (parse l) (And (parse c) (parse r))) ors
      in  foldl (Or) (Pack F.idOr) ani
  where
  quotedwords :: String -> [String]
  quotedwords = words


parseItem :: String -> Fexpr F.Filter
parseItem s
  | "content=" `isPrefixOf` s   = Content . getValue $ s
  | "title=" `isPrefixOf` s     = Title . getValue $ s
  | "state=" `isPrefixOf` s     = State . getValue $ s
  | head s == '+'               = Tag . tail $ s
  | head s == '-'               = Notag . tail $ s
  | otherwise                   = Pack F.idAnd
  where getValue :: String -> String
        getValue = mayStripSingleQuote . tail . dropWhile (/= '=')

mayStripSingleQuote :: String -> String
mayStripSingleQuote s =
  if head s == "'" !! 0 && last s == "'" !! 0
    then tail $ init s
    else s

isWithParens :: String -> Bool
isWithParens s = ('(' `elem` s) && (')' `elem` s)

isWithORs :: String -> Bool
isWithORs = isInfixOf "OR"


extractOuterORs :: String -> [String]
extractOuterORs = extractOutOrsH 0 "" []
  where
  extractOutOrsH :: Int       -- current bracket level
                 -> String    -- word accumulator
                 -> [String]  -- list accumulator (result)
                 -> String    -- to be processed
                 -> [String]
  extractOutOrsH _ ac acc [] = acc ++ [ac]
  extractOutOrsH _ ac acc [s] = acc ++ [ac ++ [s]]
  extractOutOrsH n ac acc (s:t:ss) =
    case s of
      'O' -> if t == 'R' && n == 0
               then extractOutOrsH n "" (acc ++ [ac]) ss
               else extractOutOrsH n (ac ++ [s]) acc (t:ss)
      '(' -> extractOutOrsH (n + 1) (ac ++ [s]) acc (t:ss)
      ')' -> extractOutOrsH (n - 1) (ac ++ [s]) acc (t:ss)
      _ -> extractOutOrsH n (ac ++ [s]) acc (t:ss)


extractLeftOuterParen :: String -> (String, String, String)
extractLeftOuterParen = extractH 0 ("", "")
  where
  extractH :: Int              -- current bracket level
           -> (String, String) -- accumulated result
           -> String           -- not yet processed string
           -> (String, String, String)
  extractH n acc "" =
    if n == 0
      then (fst acc, snd acc, "")
      else ("", "", "")
  extractH n (l, c) t =
    if head t == ')'
      then
        if n == 1
          then (l, tail c, tail t)
          else extractH (n - 1) (l, c ++ ")") $ tail t
      else
        if head t == '('
          then extractH (n + 1) (l, c ++ "(") $ tail t
          else 
            if n == 0
              then extractH n (l ++ [head t], c) $ tail t
              else extractH n (l, c ++ [head t]) $ tail t

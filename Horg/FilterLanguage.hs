{-# LANGUAGE GADTs #-}
module Horg.FilterLanguage where

import qualified Data.Text as T
import Data.List
import Data.Time.LocalTime (LocalTime)
import Control.Applicative ((<$>))

import qualified Horg.Filter as F
import qualified Horg.Datetime as DT
import qualified Horg.Heading as H
import Helpers (justFilter)
-- import qualified Parse as P


data Fexpr a where
    Pack :: F.Filter -> Fexpr F.Filter
    Tag :: String -> Fexpr F.Filter
    Notag :: String -> Fexpr F.Filter
    Content :: String -> Fexpr F.Filter
    Title :: String -> Fexpr F.Filter
    State :: String -> Fexpr F.Filter
    And :: Fexpr a -> Fexpr a -> Fexpr a
    Or :: Fexpr a -> Fexpr a -> Fexpr a
    EqualTimed :: H.Datetype LocalTime -> Fexpr F.Filter
    EarlierTimed :: H.Datetype LocalTime -> Fexpr F.Filter
    LaterTimed :: H.Datetype LocalTime -> Fexpr F.Filter


feval :: Fexpr F.Filter -> F.Filter
feval (Pack f) = f
feval (Tag t) = F.tag $ T.pack t
feval (Notag t) = F.tag $ T.pack t
feval (Content c) = F.content $ T.pack c
feval (Title t) = F.title $ T.pack t
feval (State s) = F.state $ T.pack s
feval (And e1 e2) = feval e1 F.*& feval e2
feval (Or e1 e2) = feval e1 F.*| feval e2
feval (EqualTimed d) = F.equaltimed d
feval (EarlierTimed d) = F.earliertimed d
feval (LaterTimed d) = F.latertimed d


-- preparse :: String -> [P.Parsed T.Text]
-- preparse = P.parse . T.pack
-- 
-- parsepreparsed :: [P.Parsed T.Text] -> Fexpr F.Filter
-- parsepreparsed = foldl (And) (Pack F.idAnd) . map parsepreparsed'
--   where
--   parsepreparsed' :: P.Parsed T.Text -> Fexpr F.Filter
--   parsepreparsed' (P.SQuote t) = undefined
--   parsepreparsed' (P.DQuote t) = undefined
--   parsepreparsed' (P.Parens t) = undefined
--   parsepreparsed' (P.Brackets t) = undefined
--   parsepreparsed' _ = undefined

parse :: String -> Fexpr F.Filter
parse t
  | not (isWithORs t || isWithParens t) =
    foldl And (Pack F.idAnd) $ justFilter $ map parseItem (quotedwords t)
  | otherwise =
      let ors = map extractLeftOuterParen $ extractOuterORs t
          ani = map (\(l, c, r) -> And (parse l) (And (parse c) (parse r))) ors
      in  foldl Or (Pack F.idOr) ani
  where
  quotedwords :: String -> [String]
  quotedwords = words


parseItem :: String -> Maybe (Fexpr F.Filter)
parseItem s
  | "content=" `isPrefixOf` s   = Just . Content . getValue $ s
  | "title=" `isPrefixOf` s     = Just . Title . getValue $ s
  | "state=" `isPrefixOf` s     = Just . State . getValue $ s
  | head s == '+'               = Just . Tag . tail $ s
  | head s == '-'               = Just . Notag . tail $ s
  | "T>" `isPrefixOf` s         = LaterTimed <$> grabDate H.Timestamp s
  | "T<" `isPrefixOf` s         = EarlierTimed <$> grabDate H.Timestamp s
  | "T=" `isPrefixOf` s         = EqualTimed <$> grabDate H.Timestamp s
  | "S>" `isPrefixOf` s         = LaterTimed <$> grabDate H.Scheduled s
  | "S<" `isPrefixOf` s         = EarlierTimed <$> grabDate H.Scheduled s
  | "S=" `isPrefixOf` s         = EqualTimed <$> grabDate H.Scheduled s
  | "C>" `isPrefixOf` s         = LaterTimed <$> grabDate H.Closed s
  | "C<" `isPrefixOf` s         = EarlierTimed <$> grabDate H.Closed s
  | "C=" `isPrefixOf` s         = EqualTimed <$> grabDate H.Closed s
  | "D>" `isPrefixOf` s         = LaterTimed <$> grabDate H.Deadline s
  | "D<" `isPrefixOf` s         = EarlierTimed <$> grabDate H.Deadline s
  | "D=" `isPrefixOf` s         = EqualTimed <$> grabDate H.Deadline s
  | otherwise                   = Just . Pack $ F.idAnd
  where getValue :: String -> String
        getValue = mayStripSingleQuote . tail . dropWhile (/= '=')

grabDate :: (LocalTime -> H.Datetype LocalTime)
         -> String -> Maybe (H.Datetype LocalTime)
grabDate f ss = f <$> (DT.parseDate . tail . tail $ ss)

mayStripSingleQuote :: String -> String
mayStripSingleQuote s =
  if head s == head "'" && last s == head "'"
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
  extractH n (l, c) t
    | head t == ')' =
        if n == 1
          then (l, tail c, tail t)
          else extractH (n - 1) (l, c ++ ")") $ tail t
    | head t == '(' =
        extractH (n + 1) (l, c ++ "(") $ tail t
    | otherwise =
        if n == 0
          then extractH n (l ++ [head t], c) $ tail t
          else extractH n (l, c ++ [head t]) $ tail t

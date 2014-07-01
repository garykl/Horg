module Parse where

import Data.Text as T

data Parsed a = SQuote a
              | DQuote a
              | Token a
              | Parens [Parsed a]
              | Chevrons [Parsed a]
              | Brackets [Parsed a]
              | Braces [Parsed a] deriving Show


takeNth :: Int -> Char -> T.Text -> T.Text
takeNth n c t = takeNth' t c n T.empty
  where
  takeNth' :: T.Text -> Char -> Int -> T.Text -> T.Text
  takeNth' _ _ 0 acc = T.init acc
  takeNth' tt cc nn acc =
      if T.head tt == cc
         then takeNth' (T.tail tt) cc (nn - 1)
                       (T.append acc $ T.singleton $ T.head tt)
         else takeNth' (T.tail tt) cc nn
                       (T.append acc $ T.singleton $ T.head tt)

dropNth :: Int -> Char -> T.Text -> T.Text
dropNth n c t = dropNth' t c n
  where
  dropNth' :: T.Text -> Char -> Int -> T.Text
  dropNth' tt _ 0 = tt
  dropNth' tt cc nn =
      if T.head tt == cc
         then dropNth' (T.tail tt) cc (nn - 1)
         else dropNth' (T.tail tt) cc nn


deepness :: Char -> Char -> T.Text -> Int
deepness c1 c2 = deepness' (0, 0)
  where
  deepness' :: (Int, Int) -> T.Text -> Int
  deepness' (n1, n2) tt =
    if tt == T.empty then n1
      else if T.head tt == c1 then
             if n2 + 1 > n1
                        then deepness' (n1 + 1, n2 + 1) (T.tail tt)
                        else deepness' (n1, n2 + 1) (T.tail tt)
             else if T.head tt == c2 then
                    deepness' (n1, n2 - 1) (T.tail tt)
                  else deepness' (n1, n2) (T.tail tt)


telem :: Char -> T.Text -> Bool
telem c t = T.singleton c `T.isInfixOf` t

parse :: T.Text -> [Parsed T.Text]
parse t =
    let dp = deepness '(' ')' t
        dc = deepness '<' '>' t
        db1 = deepness '[' ']' t
        db2 = deepness '{' '}' t
    in  if dp == 0 && dc == 0 && db1 == 0 && db2 == 0
           && (not . telem '\'') t && (not . telem '"') t
            then mayadd t []
            else parseH dp dc db1 db2 T.empty t
    where parseH :: Int -> Int -> Int -> Int
                 -> T.Text -> T.Text -> [Parsed T.Text]
          parseH parens chevrons brackets braces acc tt =
            if T.null tt then [] else
              case T.head tt of
                '\'' -> let h = T.takeWhile (/= '\'') $ T.tail tt
                        in  mayadd acc $ SQuote h :
                              parse (T.tail . T.dropWhile (/= '\'') $ T.tail tt)
                '"' -> let h = T.takeWhile (/= '"') $ T.tail tt
                       in  mayadd acc $ DQuote h :
                              parse (T.tail . T.dropWhile (/= '"') $ T.tail tt)
                '(' -> mayadd acc $
                         Parens (parse (takeNth parens ')' $ T.tail tt)) :
                         (parse $ dropNth parens ')' $ T.tail tt)
                '<' -> mayadd acc $
                         Chevrons (parse (takeNth chevrons '>' $ T.tail tt)) :
                         (parse $ dropNth chevrons '>' $ T.tail tt)
                '[' -> mayadd acc $
                         Brackets (parse (takeNth brackets ']' $ T.tail tt)) :
                         (parse $ dropNth brackets ']' $ T.tail tt)
                '{' -> mayadd acc $
                         Braces (parse (takeNth braces '}' $ T.tail tt)) :
                         (parse $ dropNth braces '}' $ T.tail tt)
                _ -> parseH parens chevrons brackets braces
                           (T.append acc $ T.singleton . T.head $ tt)
                           (T.tail tt)

mayadd :: T.Text -> [Parsed T.Text] -> [Parsed T.Text]
mayadd t ps = if T.null t then ps else Token (T.strip t) : ps

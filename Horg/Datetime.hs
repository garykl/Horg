module Horg.Datetime where

import Data.List.Split (splitOn)
import Data.Time.Format (parseTime, formatTime)
import Data.Time.LocalTime (LocalTime)
import System.Locale (defaultTimeLocale)

(|>>) :: (a -> Maybe b) -> (a -> Maybe b) -> (a -> Maybe b)
f |>> g =
    \a -> case f a of
              Nothing -> g a
              Just r -> Just r

shortDateFormat :: String
shortDateFormat = "<%Y-%m-%d>"

longDateFormat :: String
longDateFormat = "<%Y-%m-%d %a>"

shortDateTimeFormat :: String
shortDateTimeFormat = "<%Y-%m-%d %H:%M>"

longDateTimeFormat :: String
longDateTimeFormat = "<%Y-%m-%d %a %H:%M>"

iShortDateFormat :: String
iShortDateFormat = "[%Y-%m-%d]"

iLongDateFormat :: String
iLongDateFormat = "[%Y-%m-%d %a]"

iShortDateTimeFormat :: String
iShortDateTimeFormat = "[%Y-%m-%d %H:%M]"

iLongDateTimeFormat :: String
iLongDateTimeFormat = "[%Y-%m-%d %a %H:%M]"

parseDate :: String -> Maybe LocalTime
parseDate = foldl1 (|>>) $
                map parse [longDateTimeFormat,
                           shortDateTimeFormat,
                           longDateFormat,
                           shortDateFormat,
                           iLongDateTimeFormat,
                           iShortDateTimeFormat,
                           iLongDateFormat,
                           iShortDateFormat]


parseDateTimeRange :: String -> Maybe (LocalTime, LocalTime)
parseDateTimeRange s =
    let w = words s
        timerange = takeWhile (/= '>') $ last w
        restdate = unwords $ init w
        dateS1 = restdate ++ " " ++ (take 5 timerange) ++ ">"
        dateS2 = restdate ++ " " ++ (take 5 $ drop 6 timerange) ++ ">"
        date1 = parseDate dateS1
        date2 = parseDate dateS2
    in  case date1 of
            Nothing -> Nothing
            Just d1 -> case date2 of
                           Nothing -> Nothing
                           Just d2 -> Just (d1, d2)

parseDateRange :: String -> Maybe (LocalTime, LocalTime)
parseDateRange s =
    let [date1, date2] = splitOn "--" s
    in  case parseDate date1 of
            Nothing -> Nothing
            Just d1 -> case parseDate date2 of
                           Nothing -> Nothing
                           Just d2 -> Just (d1, d2)


parse :: String -> String -> Maybe LocalTime
parse = parseTime defaultTimeLocale

showLocalTime :: LocalTime -> String
showLocalTime = formatTime defaultTimeLocale longDateTimeFormat

showRange :: (LocalTime, LocalTime) -> String
showRange (d1, d2) = showLocalTime d1 ++ "--" ++ showLocalTime d2

module Main where

import System.Environment (getArgs)
import qualified Data.Text as T

import HeadingShore (allHeaders, showHeading, parseFile)


main :: IO ()
main = do

    putStrLn banner

    args <- getArgs
    mayn $ getFilename args

mayn :: Maybe FilePath -> IO ()
mayn Nothing = putStrLn "no file given"
mayn (Just fn) = justMain fn


justMain :: FilePath -> IO ()
justMain fn = do
    cntnt <- readFile fn
    let gg = parseFile cntnt
    putStrLn $ T.unpack . T.concat $ map showHeading gg

testAllHeaders :: FilePath -> IO ()
testAllHeaders fn = do
    cntnt <- readFile fn
    let gg = allHeaders (T.lines $ T.pack cntnt) 1
    putStrLn $ show $ allHeaders
            (drop 5 $ (allHeaders (drop 3 $ gg !! 0) 2) !! 0)
            3

getFilename :: [String] -> Maybe FilePath
getFilename args | null args  = Nothing
                 | otherwise   = Just $ head args




banner :: String
banner = ">>>========<<<\n"
      ++ ">>-  Horg  -<<\n"
      ++ ">>>========<<<\n"

module Main where

import System.Environment (getArgs)
import qualified Data.Text as T

import qualified Horg.Parse as Parse
import qualified Horg.Output.Org as Org


main :: IO ()
main = do

    putStrLn banner

    args <- getArgs
    mapM_ justMain args


justMain :: FilePath -> IO ()
justMain fn = do
    cntnt <- readFile fn
    let gg = Parse.parseFile cntnt
    putStrLn $ T.unpack . T.concat $ map Org.showHeading gg


banner :: String
banner = ">>>========<<<\n"
      ++ ">>-  Horg  -<<\n"
      ++ ">>>========<<<\n"

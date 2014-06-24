module Main where

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Set as S

import qualified Horg.Parse as Parse
import qualified Horg.Output.Org as Org
import qualified Horg.Heading as Heading


main :: IO ()
main = do

    putStrLn banner

    args <- getArgs
    mapM_ justTags args

justTags :: FilePath -> IO ()
justTags fn = do
    cntnt <- readFile fn
    let gg = Parse.parseFile cntnt
    print $ S.toList . S.unions . (map Heading.collectTags) $ gg

justMain :: FilePath -> IO ()
justMain fn = do
    cntnt <- readFile fn
    let gg = Parse.parseFile cntnt
    putStrLn $ T.unpack . T.concat $ map Org.showHeading gg


banner :: String
banner = "    >>================<<\n"
      ++ "  >>=---=/   -   \\=- --=<<\n"
      ++ ">>------<   Horg   >------<<\n"
      ++ "  >>=-- -=\\   -   /=---=<<\n"
      ++ "    >>================<<\n"

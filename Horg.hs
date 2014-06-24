module Main where

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Set as S

import qualified Horg.Parse as Parse
-- import qualified Horg.Output.Org as Org
import qualified Horg.Output.Dot as Dot
import qualified Horg.Heading as Heading


main :: IO ()
main = do

    --putStrLn banner

    args <- getArgs
    justMain args

justTags :: FilePath -> IO ()
justTags fn = do
    cntnt <- readFile fn
    let gg = Parse.parseFile cntnt
    print $ S.toList . S.unions . (map Heading.collectTags) $ gg

justMain :: [FilePath] -> IO ()
justMain fn = do
    cntnt <- mapM readFile fn
    let gg = concat $ map Parse.parseFile cntnt
    putStrLn $ T.unpack $ Dot.showHeadings gg


banner :: String
banner = "    >>================<<\n"
      ++ "  >>=---=/   -   \\=- --=<<\n"
      ++ ">>------<   Horg   >------<<\n"
      ++ "  >>=-- -=\\   -   /=---=<<\n"
      ++ "    >>================<<\n"

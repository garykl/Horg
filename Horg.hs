module Main where

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Set as S

import qualified Horg.Parse as Parse
import qualified Horg.Filter as Filter
import qualified Horg.FilterLanguage as FilterLanguage
-- import qualified Horg.Output.Org as Org
import qualified Horg.Output.Dot as Dot
import qualified Horg.Output.DotConf as DotConf
import qualified Horg.Heading as Heading


main :: IO ()
main = do

    --putStrLn banner

    args <- getArgs
    let filterexpression = unwords . takeWhile (/= "--") $ args
    print filterexpression
    let filenames = tail . dropWhile (/= "--") $ args
    let filt = FilterLanguage.feval . FilterLanguage.parse $ filterexpression
    justMain filenames filt

justTags :: FilePath -> IO ()
justTags fn = do
    cntnt <- readFile fn
    let gg = Parse.parseFile cntnt
    print $ S.toList . S.unions . (map Heading.collectTags) $ gg

justMain :: [FilePath] -> Filter.Filter -> IO ()
justMain fn filt = do
    cntnt <- mapM readFile fn
    let hs = concat $ map Parse.parseFile cntnt
        filtered = concat . map (Filter.deep filt) $ hs
    putStrLn $ T.unpack $ Dot.showHeadings DotConf.defaultConf filtered


banner :: String
banner = "    >>================<<\n"
      ++ "  >>=---=/   -   \\=- --=<<\n"
      ++ ">>------<   Horg   >------<<\n"
      ++ "  >>=-- -=\\   -   /=---=<<\n"
      ++ "    >>================<<\n"

module Main where

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.List as L

import qualified Horg.Parse as Parse
import qualified Horg.Filter as Filter
import qualified Horg.FilterLanguage as FilterLanguage
import qualified Horg.Output.Org as Org
import qualified Horg.Output.Dot as Dot
import qualified Horg.Output.DotConf as DotConf
import qualified Horg.Heading as Heading


main :: IO ()
main = do

    --putStrLn banner

    args <- getArgs
    let options = takeWhile (/= "--") $ args
    let filterexpression = unwords $ filter (not . optionsfilter) options
    let restoptions = filter optionsfilter options
    let filenames = tail . dropWhile (/= "--") $ args
    let filt = FilterLanguage.feval . FilterLanguage.parse $ filterexpression
    let fullfilter = extractfilterfromoptions restoptions filt
    headings <- getHeadings filenames fullfilter
    let tobeshown =
          if extractcontentflagfromoptions restoptions
            then headings
            else map (Heading.traverse Heading.removeContent) headings
    putStrLn . T.unpack . extractoutputfromoptions restoptions $ tobeshown


optionsfilter :: String -> Bool
optionsfilter s
    | "-filter=" `L.isPrefixOf` s     = True
    | "-nocontent" `L.isPrefixOf` s  = True
    | "-output=" `L.isPrefixOf` s  = True
    | otherwise                       = False

defaultfilter :: Filter.Filter -> Heading.Heading -> [Heading.Heading]
defaultfilter = Filter.deep

extractfilterfromoptions :: [String] -> Filter.Filter -> Heading.Heading -> [Heading.Heading]
extractfilterfromoptions ss =
    let keyword = "filter"
        s = filter (L.isPrefixOf $ "-" ++ keyword ++ "=") $ ss
    in  if null s then defaultfilter
        else
          case drop ((length keyword) + 2) $ head s of
              "deep" -> Filter.deep
              "surface" -> Filter.surface
              "conserve" -> Filter.conserve
              "preserve" -> Filter.preserve
              _ -> defaultfilter

defaultoutput :: [Heading.Heading] -> T.Text
defaultoutput = T.concat . map Org.showHeading

extractoutputfromoptions :: [String] -> [Heading.Heading] -> T.Text
extractoutputfromoptions ss =
    let keyword = "output"
        s = filter (L.isPrefixOf $ "-" ++ keyword ++ "=") $ ss
    in  if null s then defaultoutput
        else
          case drop ((length keyword) + 2) $ head s of
              "org" -> T.concat . map Org.showHeading
              "dot" -> Dot.showHeadings DotConf.defaultConf
              _ -> defaultoutput

extractcontentflagfromoptions :: [String] -> Bool
extractcontentflagfromoptions ss =
    let keyword = "nocontent"
        s = filter (L.isPrefixOf $ "-" ++ keyword) $ ss
    in  null s

justTags :: FilePath -> IO ()
justTags fn = do
    cntnt <- readFile fn
    let gg = Parse.parseFile cntnt
    print $ S.toList . S.unions . (map Heading.collectTags) $ gg


justDot :: [FilePath] -> Filter.Filter -> IO ()
justDot fn filt = do
    cntnt <- mapM readFile fn
    let hs = concat $ map Parse.parseFile cntnt
        filtered = concat . map (Filter.deep filt) $ hs
    putStrLn $ T.unpack $ Dot.showHeadings DotConf.defaultConf filtered

getHeadings :: [FilePath] -> (Heading.Heading -> [Heading.Heading])
            -> IO [Heading.Heading]
getHeadings fn filt = do
    cntnt <- mapM readFile fn
    let hs = concat $ map Parse.parseFile cntnt
    return $ concatMap filt hs


justOrg :: [FilePath] -> (Heading.Heading -> [Heading.Heading]) -> IO ()
justOrg fn filt = do
    cntnt <- mapM readFile fn
    let hs = concat $ map Parse.parseFile cntnt
        filtered = concatMap filt hs
    putStrLn $ T.unpack . T.concat $ map Org.showHeading filtered


banner :: String
banner = "    >>================<<\n"
      ++ "  >>=---=/   -   \\=- --=<<\n"
      ++ ">>------<   Horg   >------<<\n"
      ++ "  >>=-- -=\\   -   /=---=<<\n"
      ++ "    >>================<<\n"

module Main where

import GHC.Base (join)
import System.Directory (listDirectory, doesFileExist)
import System.Environment (getArgs)
import Text.RegexPR
import Data.List (intercalate)
import Data.Set (fromList, toList)
import Data.String.Utils (strip, endswith, replace)

foo :: FilePath -> IO [FilePath]
foo path = do
        isFile <- doesFileExist path
        if isFile
        then return [path]
        else walk path

walk :: FilePath -> IO [FilePath]
walk path = do
        entries <- listDirectory path
        let x = map foo (map (\x -> path ++ "/" ++ x) entries)
        let y = sequence x
        z <- y
        let h = join z
        return h

replaceImport :: String -> String -> String
replaceImport to from = subRegexPR "import\\s+\\*\\s+as\\s+THREE\\s+from\\s+(\"|')three(\"|').*" to from

parseAllTHREEcomponents :: String -> [String]
parseAllTHREEcomponents = map (\((_,_), ((_,m):_)) -> m) . gmatchRegexPR "THREE\\.(.+?)[^a-zA-Z0-9_]"

removeTHREEprefix :: String -> String
removeTHREEprefix = gsubRegexPR "THREE\\.(\\w+?)" "\\1"

makeNewImport :: [String] -> String
makeNewImport xs = if length xs == 0 then "" else "import {" ++ (intercalate ", " (toList $ fromList xs)) ++ "} from \"three\";\n"



pipe :: String -> String
pipe content =
        let
        components = parseAllTHREEcomponents content
        in
        replaceImport (makeNewImport components) $ removeTHREEprefix content



processFile :: FilePath -> IO ()
processFile path = do
        contents <- readFile path
        writeFile "dummy" contents
        writeFile path $ pipe contents

processFiles :: [FilePath] -> IO ()
processFiles (head:tail) = (processFile head) >> processFiles tail
processFiles [] = return ()

mainTHREE :: IO ()
mainTHREE = do
        (path:_) <- getArgs
        files <- walk path
        let jsFiles = filter ((==) "sj." . take 3 . reverse ) files
        processFiles jsFiles


findPair :: [String] -> [String] -> [(String, String)]
findPair xs ys = do
        x <- xs
        y <- map (replace "/Users/ingunjon/Desktop/three.js" "three") ys
        if endswith ( "/" ++ x ++ ".js") y then [(x,y)] else []


mainGenImports :: IO()
mainGenImports = do
        c <- getContents
        let uniqueSet = toList $ fromList $ map strip $ lines c
        (path:_) <- getArgs
        files <- walk path
        let pairs = toList $ fromList $ findPair uniqueSet files
        let aaa = map (\(x,y) -> "import {" ++ x ++ "} from \"" ++ y ++ "\";") pairs
        putStrLn $ unlines aaa

toBasicTHREEImport :: FilePath -> IO()
toBasicTHREEImport path = do
        contents <- readFile path
        writeFile "dummy" contents
        let subed = gsubRegexPR "import(.+)from \"three/src.+" "import \\1from \"three\";" contents
        writeFile path subed
mainAA :: IO()
mainAA = do
        (path:_) <- getArgs
        files <- walk path
        let jsFiles = filter (endswith ".js") files
        sequence $ map toBasicTHREEImport jsFiles
        return ()

matchDouble :: String -> String
matchDouble str = let m = matchRegexPR "\\d+\\.\\d+" str
                  in case m of Just ((a, _), _) -> a
                               _ -> undefined

toDouble :: String -> Double
toDouble = read

main :: IO()
main = do
        c <- getContents
        let comb = show . sum . map (toDouble . matchDouble) . lines
        putStrLn $ comb c

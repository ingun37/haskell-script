module Main where

import GHC.Base (join)
import System.Directory (listDirectory, doesFileExist)
import System.Environment (getArgs)
import Text.RegexPR (matchRegexPR, subRegexPR)
import System.IO

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
               

matchImport content = matchRegexPR "import\\s+\\*\\s+as\\s+THREE\\s+from\\s+\"three\";$" content

getMatch :: Maybe ((String, (String, String)), [(Int, String)]) -> Maybe String
getMatch = fmap (\((m,_), _) -> m)

removeImport :: String -> String
removeImport = subRegexPR "import\\s+\\*\\s+as\\s+THREE\\s+from\\s+\"three\";$" ""

parseAllTHREEcomponents :: String -> [String]
parseAllTHREEcomponents _ = []

removeTHREEprefix :: String -> String
removeTHREEprefix = id

addNewImport :: String -> [String] -> String
addNewImport x xs = x

pipe :: String -> String
pipe content =
        let
        components = parseAllTHREEcomponents content
        x = removeImport $ removeTHREEprefix content
        in
        addNewImport x components



processFile :: FilePath -> IO ()
processFile path = do
        contents <- readFile path
        putStrLn contents
        writeFile path $ pipe contents

processFiles :: [FilePath] -> IO ()
processFiles (head:tail) = (processFile head) >> processFiles tail
processFiles [] = return ()

main :: IO ()
main = do (path:_) <- getArgs
          files <- walk path
          let jsFiles = filter ((==) "sj." . take 3 . reverse ) files
          processFiles jsFiles



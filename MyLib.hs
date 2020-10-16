module MyLib (onlyMatch, onlyGroups, onlyGroup, allFiles) where

import System.Directory (listDirectory, doesFileExist)
import Control.Monad (join, (>=>))
-- matchRegexPR gmatchRegexPR
onlyMatch :: Functor m => m ((String, (String, String)), [(Int, String)]) -> m String
onlyMatch = fmap (fst . fst)

onlyGroups :: Functor m => m ((String, (String, String)), [(Int, String)]) -> m [String]
onlyGroups = fmap ((map snd) . snd)

onlyGroup :: Functor m => Int -> m ((String, (String, String)), [(Int, String)]) -> m String
onlyGroup groupIdx = fmap (!! groupIdx) . onlyGroups

_allFiles :: FilePath -> IO [FilePath]
_allFiles path = doesFileExist path >>= (\x -> if x then return [path] else allFiles path)

allFiles :: FilePath -> IO [FilePath]
allFiles path = listDirectory path >>= fmap join . sequence . map (_allFiles . ((path ++ "/") ++))
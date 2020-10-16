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
_allFiles path = doesFileExist path >>= _ifInv (return [path]) (allFiles path)

allFiles :: FilePath -> IO [FilePath]
allFiles path = listDirectory path >>= _ioJoin . map (_allFiles . ((path ++ "/") ++))

_ioJoin :: [IO[a]] -> IO [a]
_ioJoin = fmap join . sequence

_ifInv :: a -> a -> Bool -> a
_ifInv a b c = if c then a else b
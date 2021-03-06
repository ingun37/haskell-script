{-# LANGUAGE DeriveGeneric #-}

module Lib (parse) where

import Crypto.Hash.SHA1 as SHA ( hash )
import System.Directory.Tree
    ( readDirectoryWithL, DirTree(Dir, File), filterDir, zipPaths )
import Data.List.Split ()
import Data.Map ( fromList, Map )
import Data.Aeson ( ToJSON )
import qualified Data.Aeson as J
import GHC.Generics ( Generic )
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.UTF8 as BzUTF8
import System.FilePath.Posix (takeFileName, takeBaseName, (</>))
import qualified System.FilePath.Posix as P
import qualified Data.ByteString.Base16 as B16
import qualified Text.RegexPR as R
import qualified Util as U
import qualified System.Directory as D
import qualified Data.Tree as T
import Data.Tuple.Utils (dup)
import Data.Bifunctor (bimap)

data Item = Item {
    title :: String,
    sha1 :: String,
    attr :: (Map String String)
    } deriving (Generic, Show)

data Node = Node {
path :: String,
item :: Item,
kids :: [Item],
parentSha1 :: String
} deriving (Generic, Show)

sha1InHex = B.unpack . B16.encode . SHA.hash . B.pack
--it generates tree from bottom-up (dynamic programming)
makeTr :: String -> String -> [DirTree String] -> T.Tree Node
makeTr path parentSha1 entries =
    let sha1 = sha1InHex path
        kidTrs = [makeTr (path </> title) sha1 entries' | Dir title entries' <- entries]
        kidItems = Prelude.map (item . T.rootLabel) kidTrs
        thisItem = Item (takeFileName path) sha1 (fromList [(takeBaseName name', file) | File name' file <- entries])
        thisNode = Node path thisItem kidItems parentSha1
    in T.Node thisNode kidTrs

instance ToJSON Item
instance ToJSON Node

writeJson' :: String -> Node -> IO ()
writeJson' dst = let f = (dst </>) . (P.-<.> ".json") . sha1 . item -- make destination file path from Node
                     g = BzUTF8.toString . J.encode                 -- make json from Node
                 in (uncurry writeFile) . bimap f g . dup           -- dup :: a -> (a,a)

dirFilter :: DirTree a -> Bool
dirFilter (File name _) = ".md" `P.isExtensionOf` name
dirFilter _ = True

parse :: FilePath -> FilePath -> IO ()
parse src dst = do
    mdDir <- (filterDir dirFilter) . zipPaths <$> readDirectoryWithL readFile src
    let imgDst = dst </> "imgs"
    let dbDst = dst </> "db"
    let f = \x -> D.removePathForcibly x >> D.createDirectoryIfMissing True x -- clean the directory by removing and then making again
    mapM_ f [imgDst, dbDst]
    mdDir' <- traverse (mdTraverse imgDst dst) mdDir
    let (Dir name entries) = mdDir'
    mapM_ (writeJson' dbDst) (T.flatten (makeTr name "" entries))

-- change ![](a/b/c.jpg) to ![](assets/{$1}/c.jpg)
-- copy all the images to assets/$1/
mdTraverse :: FilePath -> FilePath -> (FilePath, String) -> IO String
mdTraverse assetsPath relTo (path, content) = do
    let mdDir = P.dropFileName path
    let matches = U.onlyGroup 0 $ R.gmatchRegexPR "!\\[\\]\\((?!http)(.+?)\\)" content
    mapM_ (D.createDirectoryIfMissing True . (assetsPath </>) . P.dropFileName) matches
    mapM_ (\m -> D.copyFile (mdDir </> m) (assetsPath </> m)) matches
    let rel = P.makeRelative relTo assetsPath
    return $ R.gsubRegexPR "!\\[\\]\\((?!http)(.+?)\\)" ("![](assets/"++rel++"/\\1)") content
module Main where

import qualified MyLib
import Text.RegexPR
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "" $ do
    it "gets only the match" $ do
      let x = MyLib.onlyMatch $ matchRegexPR "ab(cde)f\\1" "kkkabcdefcdefgh"
      x `shouldBe` Just "abcdefcde"

    it "gets only the first group" $ do
      let x = MyLib.onlyGroup 0 $ matchRegexPR "ab(cde)f\\1" "kkkabcdefcdefgh"
      x `shouldBe` Just "cde"
    
    
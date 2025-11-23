module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "My Tests" $ do
    it "True is True" $
      True `shouldBe` True

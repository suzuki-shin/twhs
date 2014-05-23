module TwhsSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Web.Twitter.Twhs

spec :: Spec
spec = do
  describe "hoge" $ do
    it "return hoge" $ do
      "hoge" `shouldBe` "hoge"

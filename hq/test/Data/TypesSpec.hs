module Data.TypesSpec (spec) where

import Data.Types (UserId (..))
import Serialize.Str
import Test.Hspec

spec :: Spec
spec = describe "userId StringTrans" $ do
    it "fromStr should convert String to UserId::Discord" $ do
        fromStr "d:muffin" `shouldBe` Just (Discord "muffin")

    it "fromStr should fail conversion" $ do
        (fromStr "muffin" :: Maybe UserId) `shouldBe` Nothing

    it "toStr should convert UserId to String" $ do
        toStr (Discord "muffin") `shouldBe` "d:muffin"

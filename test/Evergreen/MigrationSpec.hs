module Evergreen.MigrationSpec (spec) where

import Test.Hspec

spec :: Spec
spec =
  describe "a test block" $
    it "asserts a condition" $
      True `shouldBe` False

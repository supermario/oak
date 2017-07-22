module Evergreen.MigrationSpec (spec) where

import Test.Hspec

spec :: Spec
spec =
  describe "Schema" $
    it "should have specs"
      False `shouldBe` True

module DayOneSpec where

  import Test.Hspec
  import DayOne

  spec :: Spec
  spec = describe "day 1" $ do
    it "calculates fuel for mass" $ calculateFuelForMass 12 `shouldBe` 2
    it "calculates required fuel" $ calculateRequiredFuel [12, 15, 18] `shouldBe` 9
    it "calculates fuel for mass and fuel" $ calculateFuelForMassAndFuel 60 `shouldBe` [18, 4]
    it "calculates required fuel for mass and fuel" $ calculateRequiredFuelForMassAndFuel [60, 60] `shouldBe` 44
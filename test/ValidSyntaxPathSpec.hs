module ValidSyntaxPathSpec
  ( spec,
  )
where

import qualified EndPoints.Lib.Category.Category as Category
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck ()

spec :: Spec
spec =
  describe "Category Check Syntax Path - OK! then path is such us 1.3.6" $ do
    it "Valid syntax path such us 1.2.3" $
      Category.validSyntaxPath "1.22.333.894.9005.690" `shouldBe` True
    it "Invalid syntax path such us 1. 2.3" $
      Category.validSyntaxPath "1.22.333.894. 9005.690" `shouldBe` False
    it "Invalid syntax path such us 1.2.3." $
      Category.validSyntaxPath "1.22.333.894.9005.690." `shouldBe` False
    it "Invalid syntax path such us 1.2..3" $
      Category.validSyntaxPath "1.22.333.894..9005.690" `shouldBe` False
    it "Invalid syntax path such us 01.2.3" $
      Category.validSyntaxPath "01.22.333.894.9005.690" `shouldBe` False
    it "Invalid syntax path such us 1.2.a" $
      Category.validSyntaxPath "1.22.333.894.9005.a" `shouldBe` False
    it "Invalid syntax path such us 001.2.3" $
      Category.validSyntaxPath "001.22.333.894.9005.690" `shouldBe` False
    it "Invalid syntax path such us .1.2.3" $
      Category.validSyntaxPath ".1.2.3" `shouldBe` False
    it "Invalid syntax path such us 1/2/3" $
      Category.validSyntaxPath "1/2/3" `shouldBe` False
    it "Invalid syntax path such us 0" $
      Category.validSyntaxPath "0" `shouldBe` False

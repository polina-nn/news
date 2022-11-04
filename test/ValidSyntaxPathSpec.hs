module ValidSyntaxPathSpec
  ( spec,
  )
where

--import Control.Monad.Identity
import qualified EndPoints.Lib.Category as Category
--import qualified Logger
--import News (AppConfig (..), DbConfig (..), Handle (..), dbHost)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck ()

spec :: Spec
spec = describe "Category Check Sintax Path - OK! then path is such us 1.3.6" $ do
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
  it "Invalid syntax path such us 1/2/3" $
    Category.validSyntaxPath "1/2/3" `shouldBe` False
  it "Invalid syntax path such us 0" $
    Category.validSyntaxPath "0" `shouldBe` False

{--
validLogicPathForAddCateg :: [DataTypes.Category] -> DataTypes.CreateCategoryRequest -> Bool
validLogicPathForAddCateg categories DataTypes.CreateCategoryRequest {DataTypes.path = "1"} = Map.null (mapCategory categories) || False
validLogicPathForAddCateg categories DataTypes.CreateCategoryRequest {..} = Map.member (previosPath path (length $ wordsPath path)) catMap || Map.member path catMap
  where
    catMap = mapCategory categories
--}
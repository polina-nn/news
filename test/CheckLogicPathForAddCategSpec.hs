{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module CheckLogicPathForAddCategSpec
  ( spec,
  )
where

import Control.Monad.Identity (Identity (Identity))
import qualified EndPoints.Lib.Category as Category
import Handle (handleSpec)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck ()
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

-- checkLogicPathForAddCateg :: Monad m => News.Handle m -> DataTypes.CreateCategoryRequest -> Either ErrorTypes.AddEditCategoryError [DataTypes.Category]
-- -> m (Either ErrorTypes.AddEditCategoryError (DataTypes.CreateCategoryRequest, [DataTypes.Category]))
spec :: Spec
spec = describe "Category Check Logic Path For Add Category - OK! Does not contain holes in the numbering " $ do
  it "Invalid Logic Path For Add Category : add path 1.1.3 in  empty table" $
    Category.checkLogicPathForAddCateg handleSpec req1 (Right myCategoryEmpty) `shouldBe` Identity (Left error1)

  it "Invalid Logic Path For Add Category : add path 1.1.8, then path 1.1.7 not exists" $
    Category.checkLogicPathForAddCateg handleSpec req2 (Right myCategory) `shouldBe` Identity (Left error1)

  it "Valid Logic Path For Add Category : add path 1 in not empty table" $
    Category.checkLogicPathForAddCateg handleSpec req3 (Right myCategory) `shouldBe` Identity (Right (req3, myCategory))

  it "Valid Logic Path For Add Category : add path 1 in empty table" $
    Category.checkLogicPathForAddCateg handleSpec req3 (Right myCategoryEmpty) `shouldBe` Identity (Right (req3, myCategoryEmpty))

  it "Valid Logic Path For Add Category : add path 1.1.7, then path 1.1.6 is exists" $
    Category.checkLogicPathForAddCateg handleSpec req4 (Right myCategory) `shouldBe` Identity (Right (req4, myCategory))

  it "Valid Logic Path For Add Category : add path 1.1.3, then path 1.1.3 is exists" $
    Category.checkLogicPathForAddCateg handleSpec req5 (Right myCategory) `shouldBe` Identity (Right (req5, myCategory))

  it "Valid Logic Path For Add Category : add path 3.1, then path 3 is exists" $
    Category.checkLogicPathForAddCateg handleSpec req6 (Right myCategory) `shouldBe` Identity (Right (req6, myCategory))

req1 :: DataTypes.CreateCategoryRequest
req1 = DataTypes.CreateCategoryRequest {path = "1.1.3", category = ""}

req2 :: DataTypes.CreateCategoryRequest
req2 = DataTypes.CreateCategoryRequest {path = "1.1.8", category = ""}

req3 :: DataTypes.CreateCategoryRequest
req3 = DataTypes.CreateCategoryRequest {path = "1", category = ""}

req4 :: DataTypes.CreateCategoryRequest
req4 = DataTypes.CreateCategoryRequest {path = "1.1.7", category = ""}

req5 :: DataTypes.CreateCategoryRequest
req5 = DataTypes.CreateCategoryRequest {path = "1.1.3", category = ""}

req6 :: DataTypes.CreateCategoryRequest
req6 = DataTypes.CreateCategoryRequest {path = "3.1", category = ""}

error1 :: ErrorTypes.AddEditCategoryError
error1 = ErrorTypes.InvalidValuePath $ ErrorTypes.InvalidContent []

myCategoryEmpty :: [DataTypes.Category]
myCategoryEmpty = []

myCategory :: [DataTypes.Category]
myCategory =
  [ DataTypes.Category {category_path = "1", category_id = 1, category_name = "1"},
    DataTypes.Category {category_path = "1.1", category_id = 3, category_name = "1.1"},
    DataTypes.Category {category_path = "1.1.1", category_id = 4, category_name = "1.1.1"},
    DataTypes.Category {category_path = "1.1.2", category_id = 5, category_name = "1.1.2"},
    DataTypes.Category {category_path = "1.1.3", category_id = 14, category_name = "1.1.3"},
    DataTypes.Category {category_path = "1.1.4", category_id = 15, category_name = "1.1.4"},
    DataTypes.Category {category_path = "1.1.5", category_id = 19, category_name = "1.1.5"},
    DataTypes.Category {category_path = "1.1.6", category_id = 20, category_name = "1.1.6"},
    DataTypes.Category {category_path = "1.2", category_id = 6, category_name = "1.2"},
    DataTypes.Category {category_path = "1.3", category_id = 7, category_name = "1.3"},
    DataTypes.Category {category_path = "2", category_id = 2, category_name = "2"},
    DataTypes.Category {category_path = "2.1", category_id = 8, category_name = "2.1"},
    DataTypes.Category {category_path = "2.1.2", category_id = 9, category_name = "2.1.2"},
    DataTypes.Category {category_path = "2.1.2.1", category_id = 10, category_name = "2.1.2.1"},
    DataTypes.Category {category_path = "2.1.2.1.1", category_id = 11, category_name = "2.1.2.1.1,"},
    DataTypes.Category {category_path = "2.1.2.1.2", category_id = 16, category_name = "2.1.2.1.2"},
    DataTypes.Category {category_path = "2.2", category_id = 12, category_name = "2.2"},
    DataTypes.Category {category_path = "2.3", category_id = 13, category_name = "2.3"},
    DataTypes.Category {category_path = "2.4", category_id = 17, category_name = "2.4"},
    DataTypes.Category {category_path = "2.5", category_id = 18, category_name = "2.5"},
    DataTypes.Category {category_path = "3", category_id = 19, category_name = "3"},
    DataTypes.Category {category_path = "4", category_id = 20, category_name = "4"},
    DataTypes.Category {category_path = "5", category_id = 21, category_name = "5"}
  ]

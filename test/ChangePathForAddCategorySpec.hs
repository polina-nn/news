{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module ChangePathForAddCategorySpec
  ( spec
  ) where

import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.Category.CategoryHelpTypes as CategoryHelpTypes
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck ()
import qualified Types.DataTypes as DataTypes

spec :: Spec
spec =
  describe "Category.changePathForAddCategory:" $ do
    it "Change Path For Add Category: add path 1 in empty table" $
      Category.changePathForAddCategory req1 myCategoryEmpty `shouldBe` []
    it "Change Path For Add Category: add path 1.3.1, then path 1.3 is exists" $
      Category.changePathForAddCategory req6 myCategory `shouldBe` []
    it "Change Path For Add Category: add path 1.1.7, then path 1.1.6 is exists" $
      Category.changePathForAddCategory req3 myCategory `shouldBe` []
    it "Change Path For Add Category: add path 1.1.3, then path 1.1.3 is exists" $
      Category.changePathForAddCategory req4 myCategory `shouldBe` change4
    it
      "Change Path For Add Category: add path 2, then path 2(2.1, 2.1.1, 2.1.1.1, 3, 4, 5) are exist" $
      Category.changePathForAddCategory req2 myCategory `shouldBe` change2
    it
      "Change Path For Add Category: add path 2.1.2, then path 2.1.2, 2.1.2.1, 2.1.2.1.1, 2.1.2.1.1) are exist" $
      Category.changePathForAddCategory req5 myCategory `shouldBe` change5

req1 :: DataTypes.CreateCategoryRequest
req1 = DataTypes.CreateCategoryRequest {path = "1", category = ""}

req6 :: DataTypes.CreateCategoryRequest
req6 = DataTypes.CreateCategoryRequest {path = "1.3.1", category = ""}

req3 :: DataTypes.CreateCategoryRequest
req3 = DataTypes.CreateCategoryRequest {path = "1.1.7", category = ""}

req4 :: DataTypes.CreateCategoryRequest
req4 = DataTypes.CreateCategoryRequest {path = "1.1.3", category = ""}

change4 :: [CategoryHelpTypes.EditCategory]
change4 =
  [ CategoryHelpTypes.EditCategory {_id = 5, newPath = "1.1.4"}
  , CategoryHelpTypes.EditCategory {_id = 6, newPath = "1.1.5"}
  , CategoryHelpTypes.EditCategory {_id = 7, newPath = "1.1.6"}
  , CategoryHelpTypes.EditCategory {_id = 8, newPath = "1.1.7"}
  ]

req2 :: DataTypes.CreateCategoryRequest
req2 = DataTypes.CreateCategoryRequest {path = "2", category = ""}

change2 :: [CategoryHelpTypes.EditCategory]
change2 =
  [ CategoryHelpTypes.EditCategory {newPath = "3", _id = 11}
  , CategoryHelpTypes.EditCategory {newPath = "3.1", _id = 12}
  , CategoryHelpTypes.EditCategory {newPath = "3.1.2", _id = 13}
  , CategoryHelpTypes.EditCategory {newPath = "3.1.2.1", _id = 14}
  , CategoryHelpTypes.EditCategory {newPath = "3.1.2.1.1", _id = 15}
  , CategoryHelpTypes.EditCategory {newPath = "3.1.2.1.2", _id = 16}
  , CategoryHelpTypes.EditCategory {newPath = "3.2", _id = 17}
  , CategoryHelpTypes.EditCategory {newPath = "3.3", _id = 18}
  , CategoryHelpTypes.EditCategory {newPath = "3.4", _id = 19}
  , CategoryHelpTypes.EditCategory {newPath = "3.5", _id = 20}
  , CategoryHelpTypes.EditCategory {newPath = "4", _id = 21}
  , CategoryHelpTypes.EditCategory {newPath = "5", _id = 22}
  , CategoryHelpTypes.EditCategory {newPath = "6", _id = 23}
  ]

req5 :: DataTypes.CreateCategoryRequest
req5 = DataTypes.CreateCategoryRequest {path = "2.1.2", category = ""}

change5 :: [CategoryHelpTypes.EditCategory]
change5 =
  [ CategoryHelpTypes.EditCategory {newPath = "2.1.3", _id = 13}
  , CategoryHelpTypes.EditCategory {newPath = "2.1.3.1", _id = 14}
  , CategoryHelpTypes.EditCategory {newPath = "2.1.3.1.1", _id = 15}
  , CategoryHelpTypes.EditCategory {newPath = "2.1.3.1.2", _id = 16}
  ]

myCategoryEmpty :: [DataTypes.Category]
myCategoryEmpty = []

myCategory :: [DataTypes.Category]
myCategory =
  [ DataTypes.Category {categoryPath = "1", categoryId = 1, categoryName = "1"}
  , DataTypes.Category
      {categoryPath = "1.1", categoryId = 2, categoryName = "1.1"}
  , DataTypes.Category
      {categoryPath = "1.1.1", categoryId = 3, categoryName = "1.1.1"}
  , DataTypes.Category
      {categoryPath = "1.1.2", categoryId = 4, categoryName = "1.1.2"}
  , DataTypes.Category
      {categoryPath = "1.1.3", categoryId = 5, categoryName = "1.1.3"}
  , DataTypes.Category
      {categoryPath = "1.1.4", categoryId = 6, categoryName = "1.1.4"}
  , DataTypes.Category
      {categoryPath = "1.1.5", categoryId = 7, categoryName = "1.1.5"}
  , DataTypes.Category
      {categoryPath = "1.1.6", categoryId = 8, categoryName = "1.1.6"}
  , DataTypes.Category
      {categoryPath = "1.2", categoryId = 9, categoryName = "1.2"}
  , DataTypes.Category
      {categoryPath = "1.3", categoryId = 10, categoryName = "1.3"}
  , DataTypes.Category {categoryPath = "2", categoryId = 11, categoryName = "2"}
  , DataTypes.Category
      {categoryPath = "2.1", categoryId = 12, categoryName = "2.1"}
  , DataTypes.Category
      {categoryPath = "2.1.2", categoryId = 13, categoryName = "2.1.2"}
  , DataTypes.Category
      {categoryPath = "2.1.2.1", categoryId = 14, categoryName = "2.1.2.1"}
  , DataTypes.Category
      {categoryPath = "2.1.2.1.1", categoryId = 15, categoryName = "2.1.2.1.1,"}
  , DataTypes.Category
      {categoryPath = "2.1.2.1.2", categoryId = 16, categoryName = "2.1.2.1.2"}
  , DataTypes.Category
      {categoryPath = "2.2", categoryId = 17, categoryName = "2.2"}
  , DataTypes.Category
      {categoryPath = "2.3", categoryId = 18, categoryName = "2.3"}
  , DataTypes.Category
      {categoryPath = "2.4", categoryId = 19, categoryName = "2.4"}
  , DataTypes.Category
      {categoryPath = "2.5", categoryId = 20, categoryName = "2.5"}
  , DataTypes.Category {categoryPath = "3", categoryId = 21, categoryName = "3"}
  , DataTypes.Category {categoryPath = "4", categoryId = 22, categoryName = "4"}
  , DataTypes.Category {categoryPath = "5", categoryId = 23, categoryName = "5"}
  ]

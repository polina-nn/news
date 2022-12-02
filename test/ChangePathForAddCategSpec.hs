{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module ChangePathForAddCategSpec
  ( spec,
  )
where

import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.Category.CategoryHelpTypes as CategoryHelpTypes
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck ()
import qualified Types.DataTypes as DataTypes

spec :: Spec
spec = describe "Category.changePathForAddCateg:" $ do
  it "Change Path For Add Category: add path 1 in empty table" $
    Category.changePathForAddCateg req1 myCategoryEmpty `shouldBe` []

  it "Change Path For Add Category: add path 1.3.1, then path 1.3 is exists" $
    Category.changePathForAddCateg req6 myCategory `shouldBe` []

  it "Change Path For Add Category: add path 1.1.7, then path 1.1.6 is exists" $
    Category.changePathForAddCateg req3 myCategory `shouldBe` []

  it "Change Path For Add Category: add path 1.1.3, then path 1.1.3 is exists" $
    Category.changePathForAddCateg req4 myCategory `shouldBe` change4

  it "Change Path For Add Category: add path 2, then path 2(2.1, 2.1.1, 2.1.1.1, 3, 4, 5) are exist" $
    Category.changePathForAddCateg req2 myCategory `shouldBe` change2

  it "Change Path For Add Category: add path 2.1.2, then path 2.1.2, 2.1.2.1, 2.1.2.1.1, 2.1.2.1.1) are exist" $
    Category.changePathForAddCateg req5 myCategory `shouldBe` change5

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
  [ CategoryHelpTypes.EditCategory {_id = 5, new_path = "1.1.4"},
    CategoryHelpTypes.EditCategory {_id = 6, new_path = "1.1.5"},
    CategoryHelpTypes.EditCategory {_id = 7, new_path = "1.1.6"},
    CategoryHelpTypes.EditCategory {_id = 8, new_path = "1.1.7"}
  ]

req2 :: DataTypes.CreateCategoryRequest
req2 = DataTypes.CreateCategoryRequest {path = "2", category = ""}

change2 :: [CategoryHelpTypes.EditCategory]
change2 =
  [ CategoryHelpTypes.EditCategory {new_path = "3", _id = 11},
    CategoryHelpTypes.EditCategory {new_path = "3.1", _id = 12},
    CategoryHelpTypes.EditCategory {new_path = "3.1.2", _id = 13},
    CategoryHelpTypes.EditCategory {new_path = "3.1.2.1", _id = 14},
    CategoryHelpTypes.EditCategory {new_path = "3.1.2.1.1", _id = 15},
    CategoryHelpTypes.EditCategory {new_path = "3.1.2.1.2", _id = 16},
    CategoryHelpTypes.EditCategory {new_path = "3.2", _id = 17},
    CategoryHelpTypes.EditCategory {new_path = "3.3", _id = 18},
    CategoryHelpTypes.EditCategory {new_path = "3.4", _id = 19},
    CategoryHelpTypes.EditCategory {new_path = "3.5", _id = 20},
    CategoryHelpTypes.EditCategory {new_path = "4", _id = 21},
    CategoryHelpTypes.EditCategory {new_path = "5", _id = 22},
    CategoryHelpTypes.EditCategory {new_path = "6", _id = 23}
  ]

req5 :: DataTypes.CreateCategoryRequest
req5 = DataTypes.CreateCategoryRequest {path = "2.1.2", category = ""}

change5 :: [CategoryHelpTypes.EditCategory]
change5 =
  [ CategoryHelpTypes.EditCategory {new_path = "2.1.3", _id = 13},
    CategoryHelpTypes.EditCategory {new_path = "2.1.3.1", _id = 14},
    CategoryHelpTypes.EditCategory {new_path = "2.1.3.1.1", _id = 15},
    CategoryHelpTypes.EditCategory {new_path = "2.1.3.1.2", _id = 16}
  ]

myCategoryEmpty :: [DataTypes.Category]
myCategoryEmpty = []

myCategory :: [DataTypes.Category]
myCategory =
  [ DataTypes.Category {category_path = "1", category_id = 1, category_name = "1"},
    DataTypes.Category {category_path = "1.1", category_id = 2, category_name = "1.1"},
    DataTypes.Category {category_path = "1.1.1", category_id = 3, category_name = "1.1.1"},
    DataTypes.Category {category_path = "1.1.2", category_id = 4, category_name = "1.1.2"},
    DataTypes.Category {category_path = "1.1.3", category_id = 5, category_name = "1.1.3"},
    DataTypes.Category {category_path = "1.1.4", category_id = 6, category_name = "1.1.4"},
    DataTypes.Category {category_path = "1.1.5", category_id = 7, category_name = "1.1.5"},
    DataTypes.Category {category_path = "1.1.6", category_id = 8, category_name = "1.1.6"},
    DataTypes.Category {category_path = "1.2", category_id = 9, category_name = "1.2"},
    DataTypes.Category {category_path = "1.3", category_id = 10, category_name = "1.3"},
    DataTypes.Category {category_path = "2", category_id = 11, category_name = "2"},
    DataTypes.Category {category_path = "2.1", category_id = 12, category_name = "2.1"},
    DataTypes.Category {category_path = "2.1.2", category_id = 13, category_name = "2.1.2"},
    DataTypes.Category {category_path = "2.1.2.1", category_id = 14, category_name = "2.1.2.1"},
    DataTypes.Category {category_path = "2.1.2.1.1", category_id = 15, category_name = "2.1.2.1.1,"},
    DataTypes.Category {category_path = "2.1.2.1.2", category_id = 16, category_name = "2.1.2.1.2"},
    DataTypes.Category {category_path = "2.2", category_id = 17, category_name = "2.2"},
    DataTypes.Category {category_path = "2.3", category_id = 18, category_name = "2.3"},
    DataTypes.Category {category_path = "2.4", category_id = 19, category_name = "2.4"},
    DataTypes.Category {category_path = "2.5", category_id = 20, category_name = "2.5"},
    DataTypes.Category {category_path = "3", category_id = 21, category_name = "3"},
    DataTypes.Category {category_path = "4", category_id = 22, category_name = "4"},
    DataTypes.Category {category_path = "5", category_id = 23, category_name = "5"}
  ]

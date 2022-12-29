{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module EndPoints.Lib.ToText
  ( toText,
  )
where

import qualified Data.Text as T
import qualified EndPoints.Lib.Category.CategoryHelpTypes as CategoryHelpTypes
import qualified Types.DataTypes as DataTypes

class ToText a where
  toText :: a -> T.Text

instance ToText DataTypes.Category where
  toText DataTypes.Category {..} =
    T.concat
      [ T.pack "Category {category_path = ",
        T.pack categoryPath,
        T.pack ", category_id = ",
        T.pack $ show categoryId,
        T.pack ", category_name = ",
        categoryName,
        T.pack "} \n"
      ]

instance ToText CategoryHelpTypes.EditCategoryFullRequest where
  toText CategoryHelpTypes.EditCategoryFullRequest {..} =
    T.concat
      [ T.pack "EditCategoryFullRequest {id' = ",
        T.pack $ show id',
        T.pack ", cur_path' = ",
        T.pack curPath',
        T.pack ", cur_category' = ",
        curCategory',
        T.pack ", cur_level' = ",
        T.pack $ show curLevel',
        T.pack ", new_path' = ",
        T.pack newPath',
        T.pack ", new_category' = ",
        newCategory',
        T.pack ", new_level' = ",
        T.pack $ show newLevel',
        T.pack ", new_path_stay_after_id' = ",
        T.pack $ show newPathStayAfterId',
        T.pack "} \n"
      ]

instance ToText DataTypes.User where
  toText DataTypes.User {..} =
    T.concat
      [ T.pack "User {user_name = ",
        userName,
        T.pack ", user_login = ",
        T.pack userLogin,
        T.pack ", user_password = ",
        T.pack $ show userPassword,
        T.pack ", user_created = ",
        T.pack $ show userCreated,
        T.pack ", user_admin = ",
        T.pack $ show userAdmin,
        T.pack ", user_author = ",
        T.pack $ show userAuthor,
        T.pack "} \n"
      ]

instance ToText DataTypes.News where
  toText DataTypes.News {..} =
    T.concat
      [ T.pack "\nNews {\nnews_title = ",
        newsTitle,
        T.pack ", \nnews_created = ",
        T.pack $ show newsCreated,
        T.pack ", \nnews_author = ",
        newsAuthor,
        T.pack ", \nnews_category = ",
        T.concat $ map toText' newsCategory,
        T.pack ", \nnews_text = ",
        newsText,
        T.pack ",  \nnews_images = ",
        T.pack $ show newsImages,
        T.pack ",  \nnews_published = ",
        T.pack $ show newsPublished,
        T.pack "} \n"
      ]

toText' :: DataTypes.Category -> T.Text
toText' DataTypes.Category {..} =
  T.concat
    [ T.pack "\n     Category {category_path = ",
      T.pack categoryPath,
      T.pack ", category_id = ",
      T.pack $ show categoryId,
      T.pack ", category_name = ",
      categoryName,
      T.pack "}"
    ]

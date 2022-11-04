{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module EndPoints.Lib.ToText
  ( toText
  ) where

import qualified Data.Text as T
import qualified EndPoints.Lib.Category.CategoryHelpTypes as CategoryHelpTypes
import qualified Types.DataTypes as DataTypes

class ToText a where
  toText :: a -> T.Text

instance ToText DataTypes.Category where
  toText DataTypes.Category {..} =
    T.concat
      [ T.pack "Category {category_path = "
      , T.pack category_path
      , T.pack ", category_id = "
      , T.pack $ show category_id
      , T.pack ", category_name = "
      , category_name
      , T.pack "} \n"
      ]

instance ToText CategoryHelpTypes.EditCategoryFullRequest where
  toText CategoryHelpTypes.EditCategoryFullRequest {..} =
    T.concat
      [ T.pack "EditCategoryFullRequest {id' = "
      , T.pack $ show id'
      , T.pack ", cur_path' = "
      , T.pack cur_path'
      , T.pack ", cur_category' = "
      , cur_category'
      , T.pack ", cur_level' = "
      , T.pack $ show cur_level'
      , T.pack ", new_path' = "
      , T.pack new_path'
      , T.pack ", new_category' = "
      , new_category'
      , T.pack ", new_level' = "
      , T.pack $ show new_level'
      , T.pack ", new_path_stay_after_id' = "
      , T.pack $ show new_path_stay_after_id'
      , T.pack "} \n"
      ]

instance ToText DataTypes.User where
  toText DataTypes.User {..} =
    T.concat
      [ T.pack "User {user_name = "
      , user_name
      , T.pack ", user_login = "
      , T.pack user_login
      , T.pack ", user_password = "
      , T.pack $ show user_password
      , T.pack ", user_created = "
      , T.pack $ show user_created
      , T.pack ", user_admin = "
      , T.pack $ show user_admin
      , T.pack ", user_author = "
      , T.pack $ show user_author
      , T.pack "} \n"
      ]

instance ToText DataTypes.News where
  toText DataTypes.News {..} =
    T.concat
      [ T.pack "\nNews {\nnews_title = "
      , news_title
      , T.pack ", \nnews_created = "
      , T.pack $ show news_created
      , T.pack ", \nnews_author = "
      , news_author
      , T.pack ", \nnews_category = "
      , T.concat $ map toText' news_category
      , T.pack ", \nnews_text = "
      , news_text
      , T.pack ",  \nnews_images = "
      , T.pack $ show news_images
      , T.pack ",  \nnews_published = "
      , T.pack $ show news_published
      , T.pack "} \n"
      ]

toText' :: DataTypes.Category -> T.Text
toText' DataTypes.Category {..} =
  T.concat
    [ T.pack "\n     Category {category_path = "
    , T.pack category_path
    , T.pack ", category_id = "
    , T.pack $ show category_id
    , T.pack ", category_name = "
    , category_name
    , T.pack "}"
    ]

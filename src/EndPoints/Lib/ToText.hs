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
      [ "Category {category_path = ",
        T.pack categoryPath,
        ", category_id = ",
        T.pack $ show categoryId,
        ", category_name = ",
        categoryName,
        "} \n"
      ]

instance ToText CategoryHelpTypes.EditCategoryFullRequest where
  toText CategoryHelpTypes.EditCategoryFullRequest {..} =
    T.concat
      [ "EditCategoryFullRequest {id' = ",
        T.pack $ show id',
        ", cur_path' = ",
        T.pack curPath',
        ", cur_category' = ",
        curCategory',
        ", cur_level' = ",
        T.pack $ show curLevel',
        ", new_path' = ",
        T.pack newPath',
        ", new_category' = ",
        newCategory',
        ", new_level' = ",
        T.pack $ show newLevel',
        ", new_path_stay_after_id' = ",
        T.pack $ show newPathStayAfterId',
        "} \n"
      ]

instance ToText DataTypes.User where
  toText DataTypes.User {..} =
    T.concat
      [ "User {user_name = ",
        userName,
        ", user_login = ",
        T.pack userLogin,
        ", user_password = ",
        T.pack $ show userPassword,
        ", user_created = ",
        T.pack $ show userCreated,
        ", user_admin = ",
        T.pack $ show userAdmin,
        ", user_author = ",
        T.pack $ show userAuthor,
        "} \n"
      ]

instance ToText DataTypes.News where
  toText DataTypes.News {..} =
    T.concat
      [ "\nNews {\nnews_title = ",
        newsTitle,
        ", \nnews_created = ",
        T.pack $ show newsCreated,
        ", \nnews_author = ",
        newsAuthor,
        ", \nnews_category = ",
        T.concat $ map toText' newsCategory,
        ", \nnews_text = ",
        newsText,
        ",  \nnews_images = ",
        T.pack $ show newsImages,
        ",  \nnews_published = ",
        T.pack $ show newsPublished,
        "} \n"
      ]

toText' :: DataTypes.Category -> T.Text
toText' DataTypes.Category {..} =
  T.concat
    [ "\n     Category {category_path = ",
      T.pack categoryPath,
      ", category_id = ",
      T.pack $ show categoryId,
      ", category_name = ",
      categoryName,
      "}"
    ]

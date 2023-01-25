module EndPoints.Lib.ToText
  ( toText,
  )
where

import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified EndPoints.Lib.Category.CategoryHelpTypes as CategoryHelpTypes
import qualified Types.DataTypes as DataTypes

class ToText a where
  toText :: a -> T.Text

---------USER-------------

instance ToText DataTypes.CreateUserRequest where
  toText DataTypes.CreateUserRequest {..} =
    T.concat
      [ "CreateUserRequest {user_name = ",
        name,
        ", login = ",
        T.pack login,
        ", admin = ",
        T.pack $ show admin,
        ", author = ",
        T.pack $ show author,
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

---------IMAGE-------------

instance ToText DataTypes.CreateImageRequest where
  toText DataTypes.CreateImageRequest {..} =
    T.concat
      [ "\n     CreateImageRequest {file = ",
        file,
        ", format = ",
        T.pack format,
        ", image = ",
        T.pack image,
        "}"
      ]

-------- CATEGORY ----------------

instance ToText DataTypes.CreateCategoryRequest where
  toText DataTypes.CreateCategoryRequest {..} =
    T.concat
      [ "CreateCategoryRequest {path = ",
        T.pack path,
        ", category = ",
        category,
        "} \n"
      ]

instance ToText DataTypes.EditCategoryRequest where
  toText DataTypes.EditCategoryRequest {..} =
    T.concat
      [ "EditCategoryRequest {newPath = ",
        T.pack $ M.fromMaybe "" newPath,
        ", newCategory = ",
        M.fromMaybe "" newCategory,
        "} \n"
      ]

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

---------NEWS-------------

instance ToText DataTypes.EditNewsRequest where
  toText DataTypes.EditNewsRequest {..} =
    T.concat
      [ "\nEditNewsRequest {\nnewTitle = ",
        M.fromMaybe "" newTitle,
        ", \nnewCategoryId = ",
        T.pack $ show newCategoryId,
        ", \nnewText = ",
        M.fromMaybe "" newText,
        ",  \nnewImages = ",
        T.concat $ M.maybe [] (map toText) newImages,
        ",  \nnewPublished = ",
        T.pack $ show newPublished,
        "} \n"
      ]

instance ToText DataTypes.CreateNewsRequest where
  toText DataTypes.CreateNewsRequest {..} =
    T.concat
      [ "\nCreateNewsRequest {\ntitle = ",
        title,
        ", \nnewsCategoryId = ",
        T.pack $ show newsCategoryId,
        ", \ntext = ",
        text,
        ",  \nimages = ",
        T.concat $ M.maybe [] (map toText) images,
        ",  \npublished = ",
        T.pack $ show published,
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

instance ToText DataTypes.Filter where
  toText DataTypes.Filter {..} =
    T.concat
      [ "\nFilter {\nfilerDayAt = ",
        T.pack $ show filterDayAt,
        ", \nfilerDayUntil = ",
        T.pack $ show filterDayUntil,
        ", \nfilerDaySince = ",
        T.pack $ show filterDaySince,
        ", \nfilerAuthor  = ",
        M.fromMaybe "" filterAuthor,
        ", \nfilerCategoryId = ",
        T.pack $ show filterCategoryId,
        ",  \nfilerTitle = ",
        M.fromMaybe "" filterTitle,
        ",  \nfilerContent = ",
        M.fromMaybe "" filterContent,
        "} \n"
      ]

module EndPoints.Lib.ToText
  ( toText,
  )
where

import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Types.DataTypes as DataTypes

class ToText a where
  toText :: a -> T.Text

---------USER-------------

instance ToText DataTypes.CreateUserRequest where
  toText DataTypes.CreateUserRequest {..} =
    T.concat
      [ "CreateUserRequest {userName = ",
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
      [ "User {userName = ",
        userName,
        ", userLogin = ",
        T.pack userLogin,
        ", userPassword = ",
        T.pack $ show userPassword,
        ", userCreated = ",
        T.pack $ show userCreated,
        ", userAdmin = ",
        T.pack $ show userAdmin,
        ", userAuthor = ",
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
      [ "CreateCategoryRequest {parent = ",
        T.pack $ show parent,
        ", category = ",
        category,
        "} \n"
      ]

instance ToText DataTypes.EditCategoryRequest where
  toText DataTypes.EditCategoryRequest {..} =
    T.concat
      [ "EditCategoryRequest {newParent = ",
        T.pack $ show newParent,
        ", newCategory = ",
        M.fromMaybe "Nothing" newCategory,
        "} \n"
      ]

instance ToText DataTypes.Category where
  toText DataTypes.Category {..} =
    T.concat
      [ "Category {categoryParentId = ",
        T.pack $ show categoryParentId,
        ", categoryId = ",
        T.pack $ show categoryId,
        ", categoryName = ",
        categoryName,
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
      [ "\nNews {\nnewsTitle = ",
        newsTitle,
        ", \nnewsCreated = ",
        T.pack $ show newsCreated,
        ", \nnewsAuthor = ",
        newsAuthor,
        ", \nnewsCategory = ",
        T.concat $ map toText' newsCategory,
        ", \nnewsText = ",
        newsText,
        ",  \nnewsImages = ",
        T.pack $ show newsImages,
        ",  \nnewsPublished = ",
        T.pack $ show newsPublished,
        ",  \nnewsId = ",
        T.pack $ show newsId,
        "} \n"
      ]

toText' :: DataTypes.Category -> T.Text
toText' DataTypes.Category {..} =
  T.concat
    [ "\n     Category {categoryParentId = ",
      T.pack $ show categoryParentId,
      ", categoryId = ",
      T.pack $ show categoryId,
      ", categoryName = ",
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

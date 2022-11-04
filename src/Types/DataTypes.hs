{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | DataTypes for End Points --
module Types.DataTypes where

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Time as TIME
import GHC.Generics (Generic)
import qualified News
import Servant.API (FromHttpApiData(parseQueryParam))
import qualified Types.ErrorTypes as ErrorTypes

-- | data Db used in module DbServices
data Db =
  Db
    { _addUser :: (News.Handle IO, User, CreateUserRequest) -> IO (Either ErrorTypes.AddUserError User)
    , _addCategory :: (News.Handle IO, User, CreateCategoryRequest) -> IO (Either ErrorTypes.AddEditCategoryError Category)
    , _addNews :: (News.Handle IO, User, CreateNewsRequest) -> IO (Either ErrorTypes.AddEditNewsError News)
    , _addImage :: (News.Handle IO, User, CreateImageRequest) -> IO (Either ErrorTypes.AddImageError URI)
    , _editCategory :: (News.Handle IO, User, Int, EditCategoryRequest) -> IO (Either ErrorTypes.AddEditCategoryError Category)
    , _editNews :: (News.Handle IO, User, Int, EditNewsRequest) -> IO (Either ErrorTypes.AddEditNewsError News)
    , _authorsNewsList :: ( News.Handle IO
                          , User
                          , Filter
                          , Maybe SortBy
                          , Maybe Offset
                          , Maybe Limit) -> IO (Either ErrorTypes.GetNewsError [News])
    , _authorsNewsSearchList :: ( News.Handle IO
                                , User
                                , Maybe T.Text
                                , Maybe Offset
                                , Maybe Limit) -> IO (Either ErrorTypes.GetNewsError [News])
    , _userList :: (News.Handle IO, Maybe Offset, Maybe Limit) -> IO (Either ErrorTypes.GetContentError [User])
    , _oneImage :: (News.Handle IO, Integer) -> IO (Either ErrorTypes.GetImageError B.ByteString)
    , _categoryList :: (News.Handle IO, Maybe Offset, Maybe Limit) -> IO (Either ErrorTypes.GetContentError [Category])
    , _newsList :: ( News.Handle IO
                   , Filter
                   , Maybe SortBy
                   , Maybe Offset
                   , Maybe Limit) -> IO (Either ErrorTypes.GetNewsError [News])
    , _newsSearchList :: ( News.Handle IO
                         , Maybe T.Text
                         , Maybe Offset
                         , Maybe Limit) -> IO (Either ErrorTypes.GetNewsError [News])
    }

data SortBy
  = SortByAuthor
  | SortByCategory
  | SortByData
  | SortByFoto

instance FromHttpApiData SortBy where
  parseQueryParam value
    | T.toLower value == T.pack "author" = Right SortByAuthor
    | T.toLower value == T.pack "category" = Right SortByCategory
    | T.toLower value == T.pack "data" = Right SortByData
    | T.toLower value == T.pack "foto" = Right SortByFoto
    | otherwise = Left $ T.pack "Invalid sort_by value"

data Filter =
  Filter
    { filer_dayAt :: Maybe DayAt
    , filer_dayUntil :: Maybe DayUntil
    , filer_daySince :: Maybe DaySince
    , filer_author :: Maybe T.Text
    , filer_category_id :: Maybe Int
    , filer_title :: Maybe T.Text
    , filer_content :: Maybe T.Text
    }
  deriving (Show, Generic, Eq)

type DayAt = TIME.Day

type DayUntil = TIME.Day

type DaySince = TIME.Day

-- | type Limit -  maximum array length per get request
type Limit = Int

-- | type Offset - offset from the begining of  get response array
type Offset = Int

-- | type Id - id for user, category, image
type Id = Int

-- | type Name - name for user, category, image, news
type Name = T.Text

-- | type Path - path for category (e.g.  1.23.8.1)
type Path = String

type URI = String

---------USER-------------
-- | data CreateUserRequest - for create user in request.
-- You must fill all fields in curl request
data CreateUserRequest =
  CreateUserRequest
    { name :: Name
    , login :: String
    , password :: String
    , admin :: Bool
    , author :: Bool
    }
  deriving (Show, Generic, Eq)

instance A.ToJSON CreateUserRequest

instance A.FromJSON CreateUserRequest

data User =
  User
    { user_name :: Name -- имя пользователя
    , user_login :: String
    , user_password :: Maybe String
    , user_created :: TIME.Day
    , user_admin :: Bool
    , user_author :: Bool
    }
  deriving (Show, Generic, Eq)

instance A.ToJSON User

instance A.FromJSON User

---------IMAGE-------------
-- | CreateImage
data CreateImageRequest =
  CreateImageRequest
    { file :: Name -- имя файла
    , format :: String
    , image :: FilePath
    }
  deriving (Show, Generic, Eq)

instance A.ToJSON CreateImageRequest

instance A.FromJSON CreateImageRequest

data URI' =
  URI'
    -- | uri_Path - in request http://localhost:8080/image/11 uri_Path = image
    { uriPath :: String
    -- | id in request http://localhost:8080/image/11 uri_Id = 11
    , uriId :: Int -- image
    }

instance Show URI' where
  show (URI' uriPath' uriId') = "/" ++ uriPath' ++ "/" ++ show uriId'

-------- CATEGORY ----------------
-- | Category - one category.It has category_path, category_name, category_id (created automatically by Data Base)
data Category =
  Category
    { category_path :: Path
    , category_id :: Id
    , category_name :: Name -- имя категории
    }
  deriving (Show, Generic, Ord, Eq)

instance A.ToJSON Category

instance A.FromJSON Category

-- |  CreateCategoryRequest - for create category  in request.
-- You must fill all fields in curl request
data CreateCategoryRequest =
  CreateCategoryRequest
    { path :: Path
    , category :: Name -- имя категории
    }
  deriving (Show, Generic, Eq)

instance A.ToJSON CreateCategoryRequest

instance A.FromJSON CreateCategoryRequest

-- | EditCategoryRequest - for edit category  in request.
-- You must fill some fields in curl request
data EditCategoryRequest =
  EditCategoryRequest
    { new_path :: Maybe Path
    , new_category :: Maybe Name
    }
  deriving (Show, Generic, Eq)

instance A.ToJSON EditCategoryRequest

instance A.FromJSON EditCategoryRequest

---------NEWS-------------
data CreateNewsRequest =
  CreateNewsRequest
    { title :: Name
    , categoryId :: Id
    , text :: T.Text
    , images :: Maybe [CreateImageRequest]
    , published :: Bool
    }
  deriving (Show, Generic, Eq)

instance A.ToJSON CreateNewsRequest

instance A.FromJSON CreateNewsRequest

data EditNewsRequest =
  EditNewsRequest
    { new_title :: Maybe Name
    , new_category_id :: Maybe Id
    , new_text :: Maybe T.Text
    , new_images :: Maybe [CreateImageRequest]
    , new_published :: Maybe Bool
    }
  deriving (Show, Generic, Eq)

instance A.ToJSON EditNewsRequest

instance A.FromJSON EditNewsRequest

data News =
  News
    { news_title :: Name -- имя
    , news_created :: TIME.Day
    , news_author :: Name -- имя автора
    , news_category :: [Category] -- рекурсивно все категории
    , news_text :: T.Text
    , news_images :: [URI]
    , news_published :: Bool
    }
  deriving (Show, Generic, Eq)

instance A.ToJSON News

instance A.FromJSON News

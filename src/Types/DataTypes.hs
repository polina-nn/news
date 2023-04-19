{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | DataTypes for End Points --
module Types.DataTypes where

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Time as TIME
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (toField))
import GHC.Generics (Generic)
import qualified News
import Servant (FromHttpApiData (..))
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server.Experimental.Auth (AuthServerData)
import Text.Read (readEither)
import qualified Types.ErrorTypes as ErrorTypes

newtype Handle = Handle
  { hServerHandle :: News.Handle IO
  }

--- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "cookie-auth") = Token

-- | An Token type that we "fetch from the database" after
-- performing authentication
newtype Token = Token {token :: String}
  deriving (Show)

-- | data Db used in module DbServices
data Db = Db
  { dbAddUser :: (News.Handle IO, Token, CreateUserRequest) -> IO (Either ErrorTypes.AddUserError User),
    dbAddCategory :: (News.Handle IO, Token, CreateCategoryRequest) -> IO (Either ErrorTypes.AddEditCategoryError Category),
    dbAddNews :: (News.Handle IO, Token, CreateNewsRequest) -> IO (Either ErrorTypes.AddEditNewsError News),
    dbAddImage :: (News.Handle IO, Token, CreateImageRequest) -> IO (Either ErrorTypes.AddImageError URI),
    dbEditCategory :: (News.Handle IO, Token, Id CategoryId, EditCategoryRequest) -> IO (Either ErrorTypes.AddEditCategoryError Category),
    dbEditNews :: (News.Handle IO, Token, Id NewsId, EditNewsRequest) -> IO (Either ErrorTypes.AddEditNewsError News),
    dbAuthorsNewsList ::
      ( News.Handle IO,
        Token,
        Filter,
        Maybe SortBy,
        Maybe Offset,
        Maybe Limit
      ) ->
      IO (Either ErrorTypes.GetNewsError [News]),
    dbAuthorsNewsSearchList ::
      ( News.Handle IO,
        Token,
        Maybe T.Text,
        Maybe Offset,
        Maybe Limit
      ) ->
      IO (Either ErrorTypes.GetNewsError [News]),
    dbUserList :: (News.Handle IO, Maybe Offset, Maybe Limit) -> IO (Either ErrorTypes.GetContentError [User]),
    dbOneImage :: (News.Handle IO, Id ImageId) -> IO (Either ErrorTypes.GetImageError B.ByteString),
    dbCategoryList :: (News.Handle IO, Maybe Offset, Maybe Limit) -> IO (Either ErrorTypes.GetContentError [Category]),
    dbNewsList ::
      ( News.Handle IO,
        Filter,
        Maybe SortBy,
        Maybe Offset,
        Maybe Limit
      ) ->
      IO (Either ErrorTypes.GetNewsError [News]),
    dbNewsSearchList ::
      ( News.Handle IO,
        Maybe T.Text,
        Maybe Offset,
        Maybe Limit
      ) ->
      IO (Either ErrorTypes.GetNewsError [News])
  }

data SortBy
  = SortByAuthor
  | SortByCategory
  | SortByData
  | SortByPhoto

instance FromHttpApiData SortBy where
  parseQueryParam value
    | T.toLower value == "author" = Right SortByAuthor
    | T.toLower value == "category" = Right SortByCategory
    | T.toLower value == "data" = Right SortByData
    | T.toLower value == "photo" = Right SortByPhoto
    | otherwise = Left "Invalid sort_by value"

data Filter = Filter
  { filterDayAt :: Maybe DayAt,
    filterDayUntil :: Maybe DayUntil,
    filterDaySince :: Maybe DaySince,
    filterAuthor :: Maybe T.Text,
    filterCategoryId :: Maybe (Id CategoryId),
    filterTitle :: Maybe T.Text,
    filterContent :: Maybe T.Text
  }
  deriving (Show, Generic)

newtype DayAt = DayAt {dayAt :: TIME.Day}
  deriving (Show, Generic)

instance FromHttpApiData DayAt where
  parseUrlPiece lim =
    case readEither (T.unpack lim) :: Either String TIME.Day of
      Left err -> Left (T.pack err)
      Right val -> Right DayAt {dayAt = val}

newtype DayUntil = DayUntil {dayUntil :: TIME.Day}
  deriving (Show, Generic)

instance FromHttpApiData DayUntil where
  parseUrlPiece lim =
    case readEither (T.unpack lim) :: Either String TIME.Day of
      Left err -> Left (T.pack err)
      Right val -> Right DayUntil {dayUntil = val}

newtype DaySince = DaySince {daySince :: TIME.Day}
  deriving (Show, Generic)

instance FromHttpApiData DaySince where
  parseUrlPiece lim =
    case readEither (T.unpack lim) :: Either String TIME.Day of
      Left err -> Left (T.pack err)
      Right val -> Right DaySince {daySince = val}

-- | type Limit -  maximum array length per get request
newtype Limit = Limit {limit :: Int}
  deriving (Show, Eq, Generic)

instance FromHttpApiData Limit where
  parseUrlPiece lim =
    case readEither (T.unpack lim) :: Either String Int of
      Left err -> Left (T.pack err)
      Right val -> Right Limit {limit = val}

-- | type Offset - offset from the beginning of  get response array
newtype Offset = Offset {offset :: Int}
  deriving (Show, Eq, Generic)

instance FromHttpApiData Offset where
  parseUrlPiece off =
    case readEither (T.unpack off) :: Either String Int of
      Left err -> Left (T.pack err)
      Right val -> Right Offset {offset = val}

-- | type Name - name for user, category, image, news
type Name = T.Text

type URI = String

---------USER-------------

-- | data CreateUserRequest - for create user in request.
-- You must fill all fields in curl request
data CreateUserRequest = CreateUserRequest
  { name :: Name,
    login :: String,
    password :: String,
    admin :: Bool,
    author :: Bool
  }
  deriving (Show, Generic, Eq, A.ToJSON, A.FromJSON)

data User = User
  { userName :: Name,
    userLogin :: String,
    userPassword :: Maybe String,
    userCreated :: TIME.Day,
    userAdmin :: Bool,
    userAuthor :: Bool
  }
  deriving (Show, Generic, Eq, A.ToJSON, A.FromJSON)

-------- ID FOR CATEGORY, NEWS, IMAGE ------
data CategoryId

data NewsId

data ImageId

newtype Id a = Id {getId :: Int}
  deriving (Show, Generic, Eq, A.ToJSON, A.FromJSON, Read)

instance FromHttpApiData (Id a) where
  parseUrlPiece someId =
    case readEither (T.unpack someId) :: Either String Int of
      Left err -> Left (T.pack err)
      Right val -> Right Id {getId = val}

instance FromField (Id a) where
  fromField field' maybeData = do
    x <- fromField field' maybeData
    return Id {getId = x}

instance ToField (Id a) where
  toField (Id a) = toField a

---------IMAGE-------------

-- | CreateImage
data CreateImageRequest = CreateImageRequest
  { file :: Name,
    format :: String,
    image :: FilePath
  }
  deriving (Show, Generic, Eq, A.ToJSON, A.FromJSON)

data URI' = URI'
  { -- | uriPath - in request http://localhost:8080/image/11 uriPath = image
    uriPath :: String,
    -- | id in request http://localhost:8080/image/11 uriId = 11
    uriId :: Int -- image
  }

instance Show URI' where
  show (URI' uriPath' uriId') = "/" ++ uriPath' ++ "/" ++ show uriId'

-------- CATEGORY ----------------

-- | Category - one category.It has category_path, category_name, category_id (created automatically by Data Base)
data Category = Category
  { categoryId :: Id CategoryId,
    categoryName :: Name,
    categoryParentId :: Id CategoryId
  }
  deriving (Show, Generic, Eq, A.ToJSON, A.FromJSON)

-- |  CreateCategoryRequest - for create category  in request.
-- You must fill all fields in curl request
data CreateCategoryRequest = CreateCategoryRequest
  { parent :: Id CategoryId,
    category :: Name
  }
  deriving (Show, Generic, Eq, A.ToJSON, A.FromJSON)

-- | EditCategoryRequest - for edit category  in request.
-- You must fill some fields in curl request
data EditCategoryRequest = EditCategoryRequest
  { newParent :: Maybe (Id CategoryId),
    newCategory :: Maybe Name
  }
  deriving (Show, Generic, Eq, A.ToJSON, A.FromJSON)

---------NEWS-------------
data CreateNewsRequest = CreateNewsRequest
  { title :: Name,
    newsCategoryId :: Id CategoryId,
    text :: T.Text,
    images :: Maybe [CreateImageRequest],
    published :: Bool
  }
  deriving (Show, Generic, Eq, A.ToJSON, A.FromJSON)

data EditNewsRequest = EditNewsRequest
  { newTitle :: Maybe Name,
    newCategoryId :: Maybe (Id CategoryId),
    newText :: Maybe T.Text,
    newImages :: Maybe [CreateImageRequest],
    newPublished :: Maybe Bool
  }
  deriving (Show, Generic, Eq, A.ToJSON, A.FromJSON)

data News = News
  { newsTitle :: Name,
    newsCreated :: TIME.Day,
    newsAuthor :: Name,
    newsCategory :: [Category],
    newsText :: T.Text,
    newsImages :: [URI],
    newsPublished :: Bool,
    newsId :: Id NewsId
  }
  deriving (Show, Generic, Eq, A.ToJSON, A.FromJSON)

-- | define Errors in EndPoints
module Types.ErrorTypes where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import Logger (logError, (.<))
import qualified News

--- CODE ERRORS  --
error500 :: String
error500 = "500 Internal Server Error"

error400 :: String
error400 = "400 Bad Request"

error404 :: String
error404 = "404 Not Found"

error403 :: String
error403 = "403 Forbidden Error"

-- | COMMON ERROR LIST -- In these cases I return Error for user and LogMessage for admin
-- | 1
-- | InvalidOffset - if offset is less then 0.
-- | InvalidLimit - if limit is less or equal the  0
data InvalidOffsetOrLimit = InvalidOffset String | InvalidLimit String
  deriving (Eq)

instance Show InvalidOffsetOrLimit where
  show (InvalidOffset a) = error400 <> a
  show (InvalidLimit a) = error400 <> a

handleInvalidOffsetOrLimit :: News.Handle IO -> InvalidOffsetOrLimit -> EX.ExceptT InvalidOffsetOrLimit IO a
handleInvalidOffsetOrLimit h e = do
  liftIO $ Logger.logError (News.hLogHandle h) $ "catch InvalidOffsetOrLimit! " .< e
  EX.throwE e

-- | 3
-- | InvalidId  - Then resource with this Id in URI (ex.700) don't exists in the Data Base (http://localhost:8080/image/700)
newtype InvalidId
  = InvalidId String
  deriving (Eq)

instance Show InvalidId where
  show (InvalidId a) = error404 <> a

-- | 4
-- | InvalidContent  - Then content in request (ex.700a) is invalid. (--data '{ "new_path": "700a", "new_category": "cat " }' \ )
newtype InvalidContent
  = InvalidContent String
  deriving (Eq)

instance Show InvalidContent where
  show (InvalidContent a) = error400 <> a

-- | 5
-- | InvalidAdminPermission  - Then the user does not have admin permission to execute  request.
newtype InvalidAdminPermission
  = InvalidAdminPermission String
  deriving (Eq)

instance Show InvalidAdminPermission where
  show (InvalidAdminPermission a) = error404 <> a

handleInvalidAdminPermission :: News.Handle IO -> InvalidAdminPermission -> EX.ExceptT InvalidAdminPermission IO a
handleInvalidAdminPermission h e = do
  liftIO $ Logger.logError (News.hLogHandle h) $ "catch InvalidAdminPermission! " .< e
  EX.throwE e

-- | 6
-- | InvalidAuthorPermission  - Then the user does not have author permission to execute  request.
newtype InvalidAuthorPermission
  = InvalidAuthorPermission String
  deriving (Eq)

instance Show InvalidAuthorPermission where
  show (InvalidAuthorPermission a) = error403 <> a

handleInvalidAuthorPermission :: News.Handle IO -> InvalidAuthorPermission -> EX.ExceptT InvalidAuthorPermission IO a
handleInvalidAuthorPermission h e = do
  liftIO $ Logger.logError (News.hLogHandle h) $ "catch InvalidAuthorPermission! " .< e
  EX.throwE e

-- | 7
-- | InvalidRequest   - Then error in uri ( no word "text" in request news/search?text = "Ann")
newtype InvalidRequest
  = InvalidRequest String
  deriving (Eq)

instance Show InvalidRequest where
  show (InvalidRequest a) = error400 <> a

-- | 8
-- | SQLRequestError  - Error of executing SQL request.
newtype SQLRequestError
  = SQLRequestError String
  deriving (Eq)

instance Show SQLRequestError where
  show (SQLRequestError a) = error500 <> a

-- | 9
-- | InvalidToken  - Invalid token
newtype InvalidToken
  = InvalidToken String
  deriving (Eq)

instance Show InvalidToken where
  show (InvalidToken a) = error403 <> a

-------------------------------------------------------------

-- | LIST OF ERRORS FOR SERVER Authentication
data ServerAuthError
  = ServerAuthErrorInvalidToken InvalidToken
  | ServerAuthErrorSQLRequestError SQLRequestError
  | ServerAuthErrorSomeException EXS.SomeException
  deriving (Show, EXS.Exception)

handleServerAuthError :: News.Handle IO -> ServerAuthError -> EX.ExceptT ServerAuthError IO a
handleServerAuthError h e = do
  liftIO $ Logger.logError (News.hLogHandle h) $ "catch ServerAuthError! " .< e
  EX.throwE e

-------------------------------------------------------------

-- | LIST OF ERRORS FOR EACH ENDPOINT
-- This allows you to see the expected errors in each method.
-- | 1
-- | AddEditCategoryError - Add and edit category errors
data AddEditCategoryError
  = InvalidPermissionAddEditCategory InvalidAdminPermission
  | -- CategoryAlreadyExisted error when the category with the same name already exists. Duplication of category name is not allowed
    CategoryAlreadyExisted InvalidContent
  | -- InvalidParentId when the category parent not exists or parent is its child
    InvalidParentIdAddEditCategory InvalidContentCategoryId
  | AddEditCategorySQLRequestError SQLRequestError
  | -- InvalidCategoryId - use only in edit request
    InvalidCategoryId InvalidId
  | -- | AddEditCategoryUserError - token exists but user not
    AddEditCategorySearchUserError SearchUserError
  | AddEditCategorySomeException EXS.SomeException
  deriving (Show, EXS.Exception)

handleAddEditCategoryError :: News.Handle IO -> AddEditCategoryError -> EX.ExceptT AddEditCategoryError IO a
handleAddEditCategoryError h e = do
  liftIO $ Logger.logError (News.hLogHandle h) $ "catch AddEditCategoryError! " .< e
  EX.throwE e

-- | 2
-- | AddImageError  - Add image error
data AddImageError
  = -- | InvalidPermissionAddImage - the image is not added by the author
    InvalidPermissionAddImage InvalidAuthorPermission
  | -- | NotPngImage - the image is not {"format":"png"}
    NotPngImage InvalidContent
  | -- | NotBase64Image - the image is not Base64 coding
    NotBase64Image InvalidContent
  | -- | NotExistImageFile - does not exist file (No such file or directory) in request e.x {"image": "/Users/admin/news/_image/white_base64" }
    NotExistImageFile InvalidContent
  | -- | AddImageSearchUserError - token exists but user not
    AddImageSearchUserError SearchUserError
  | AddImageSQLRequestError SQLRequestError
  | AddImageSomeException EXS.SomeException
  deriving (Show, EXS.Exception)

handleAddImageError :: News.Handle IO -> AddImageError -> EX.ExceptT AddImageError IO a
handleAddImageError h e = do
  liftIO $ Logger.logError (News.hLogHandle h) $ "catch AddImageError! " .< e
  EX.throwE e

-- | 3
-- | AddEditNewsError  - Add or edit news error
data AddEditNewsError
  = -- | InvalidPermissionAddNews - the news is not added by the author
    InvalidPermissionAddEditNews InvalidAuthorPermission
  | -- | Add news to a non-existent category by id
    InvalidCategoryIdAddEditNews InvalidContentCategoryId
  | -- | NotPngImageAddNews - the image is not {"format":"png"}
    NotPngImageAddEditNews InvalidContent
  | -- | NotBase64ImageAddNews - the image is not Base64 coding {"image": "iVBORw0KGgoAAAANSU..""}
    NotBase64ImageAddEditNews InvalidContent
  | -- | NotExistImageFileAddNews - does not exist file (No such file or directory) in request e.x {"image": "/Users/admin/news/_image/white_base64" }
    NotExistImageFileAddEditNews InvalidContent
  | AddEditNewsSQLRequestError SQLRequestError
  | -- | AddUserSearchUserError - token exists but user not
    AddEditNewsSearchUserError SearchUserError
  | -- | InvalidNewsId - use only in edit request
    InvalidNewsId InvalidId
  | AddEditNewsSomeException EXS.SomeException
  deriving (Show, EXS.Exception)

handleAddEditNewsError :: News.Handle IO -> AddEditNewsError -> EX.ExceptT AddEditNewsError IO a
handleAddEditNewsError h e = do
  liftIO $ Logger.logError (News.hLogHandle h) $ "catch AddEditNewsError! " .< e
  EX.throwE e

-- | 4
-- | AddUserError - creation user errors
data AddUserError
  = -- |  InvalidPermissionAddUser - the user is not  admin
    InvalidPermissionAddUser InvalidAdminPermission
  | -- |  UserAlreadyExisted - this login already exists
    UserAlreadyExisted InvalidContent
  | -- | AddUserSearchUserError - token exists but user not
    AddUserSearchUserError SearchUserError
  | AddUserSQLRequestError SQLRequestError
  | AddUserSomeException EXS.SomeException
  deriving (Show, EXS.Exception)

handleAddUserError :: News.Handle IO -> AddUserError -> EX.ExceptT AddUserError IO a
handleAddUserError h e = do
  liftIO $ Logger.logError (News.hLogHandle h) $ "catch AddUserError! " .< e
  EX.throwE e

-- | 5
-- | GetNewsError - get news list error
data GetNewsError
  = InvalidOffsetOrLimitGetNews InvalidOffsetOrLimit
  | -- | InvalidFilterGetNews - only in queries with filters
    InvalidFilterGetNews InvalidRequest
  | -- | InvalidCategoryIdGetNews - when try read about news category from db
    InvalidCategoryIdGetNews InvalidContentCategoryId
  | -- | InvalidSearchGetNews  - only in search queries. Then error in uri ( no word "text" in request news/search?offset=2&limit=3)
    InvalidSearchGetNews InvalidRequest
  | -- | InvalidPermissionGetNews - only for requests with authentication
    InvalidPermissionGetNews InvalidAuthorPermission
  | -- | GetNewsSearchUserError - only for requests with authentication
    GetNewsSearchUserError SearchUserError
  | GetNewsSQLRequestError SQLRequestError
  | GetNewsSomeException EXS.SomeException
  deriving (Show, EXS.Exception)

handleGetNewsError :: News.Handle IO -> GetNewsError -> EX.ExceptT GetNewsError IO a
handleGetNewsError h e = do
  liftIO $ Logger.logError (News.hLogHandle h) $ "catch GetNewsError! " .< e
  EX.throwE e

-- | 6
-- | GetContentError - get users or category list error
data GetContentError
  = InvalidOffsetOrLimitGetContent InvalidOffsetOrLimit
  | GetContentSQLRequestError SQLRequestError
  | GetContentSomeException EXS.SomeException
  deriving (Show, EXS.Exception)

handleGetContentError :: News.Handle IO -> GetContentError -> EX.ExceptT GetContentError IO a
handleGetContentError h e = do
  liftIO $ Logger.logError (News.hLogHandle h) $ "catch GetContentError! " .< e
  EX.throwE e

-- | 7
-- | GetImageError - get one image
data GetImageError
  = InvalidImagedId InvalidId
  | GetImageSQLRequestError SQLRequestError
  | GetImageSomeException EXS.SomeException
  deriving (Show, EXS.Exception)

handleGetImageError :: News.Handle IO -> GetImageError -> EX.ExceptT GetImageError IO a
handleGetImageError h e = do
  liftIO $ Logger.logError (News.hLogHandle h) $ "catch GetImageError! " .< e
  EX.throwE e

-- | 7
-- | SearchUserError - token exists but user does not exist in data base
data SearchUserError
  = SearchUserNotExist SQLRequestError
  | SearchUserSQLRequestError SQLRequestError
  | SearchUseSomeException EXS.SomeException
  deriving (Show, EXS.Exception)

handleSearchUserError :: News.Handle IO -> SearchUserError -> EX.ExceptT SearchUserError IO a
handleSearchUserError h e = do
  liftIO $ Logger.logError (News.hLogHandle h) $ "catch SearchUserError ! " .< e
  EX.throwE e

-- | 8
-- | InvalidContentCategoryId - when the category is not valid - it does not exist, or it cannot become a parent
data InvalidContentCategoryId
  = InvalidContentCategoryIdError InvalidContent
  | InvalidContentCategoryIdSQLRequestError SQLRequestError
  | InvalidContentCategoryIdSomeException EXS.SomeException
  deriving (Show, EXS.Exception)

handleInvalidContentCategoryId :: News.Handle IO -> InvalidContentCategoryId -> EX.ExceptT InvalidContentCategoryId IO a
handleInvalidContentCategoryId h e = do
  liftIO $ Logger.logError (News.hLogHandle h) $ "catch InvalidContentCategoryId ! " .< e
  EX.throwE e

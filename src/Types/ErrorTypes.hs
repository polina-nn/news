-- | define Errors in EndPoints
module Types.ErrorTypes where

import qualified Control.Exception.Safe as EXS

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

-- | InvalidOffset - if offset is less then 0.
-- | InvalidLimit - if limit is less or equal the  0
data InvalidOffsetOrLimit = InvalidOffset String | InvalidLimit String
  deriving (Eq)

instance Show InvalidOffsetOrLimit where
  show (InvalidOffset a) = error400 <> a
  show (InvalidLimit a) = error400 <> a

-- | InvalidId  - Then resource with this Id in URI (ex.700) don't exists in the Data Base (http://localhost:8080/image/700)
newtype InvalidId
  = InvalidId String
  deriving (Eq)

instance Show InvalidId where
  show (InvalidId a) = error404 <> a

-- | InvalidContent  - Then content in request (ex.700a) is invalid. (--data '{ "new_path": "700a", "new_category": "cat " }' \ )
newtype InvalidContent
  = InvalidContent String
  deriving (Eq)

instance Show InvalidContent where
  show (InvalidContent a) = error400 <> a

-- | InvalidAdminPermission  - Then the user does not have admin permission to execute  request.
data InvalidAdminPermission
  = InvalidAdminPermission
  deriving (Eq)

instance Show InvalidAdminPermission where
  show InvalidAdminPermission = error404

-- | InvalidAuthorPermission  - Then the user does not have author permission to execute  request.
data InvalidAuthorPermission
  = InvalidAuthorPermission
  deriving (Eq)

instance Show InvalidAuthorPermission where
  show InvalidAuthorPermission = error403

-- | InvalidRequest   - Then error in uri ( no word "text" in request news/search?text = "Ann")
newtype InvalidRequest
  = InvalidRequest String
  deriving (Eq)

instance Show InvalidRequest where
  show (InvalidRequest a) = error400 <> a

-- | SQLRequestError  - Error of executing SQL request.
newtype SQLRequestError
  = SQLRequestError String
  deriving (Eq)

instance Show SQLRequestError where
  show (SQLRequestError a) = error500 <> a

-- | InvalidToken  - Invalid token
data InvalidToken
  = InvalidToken
  deriving (Eq)

instance Show InvalidToken where
  show InvalidToken = error403

-------------------------------------------------------------

-- | LIST OF ERRORS FOR SERVER Authentication
data ServerAuthError
  = ServerAuthErrorInvalidToken InvalidToken
  | ServerAuthErrorSQLRequestError SQLRequestError
  | ServerAuthSomeException EXS.SomeException
  deriving (Show, EXS.Exception)

-------------------------------------------------------------

-- | LIST OF ERRORS FOR EACH ENDPOINT
-- This allows you to see the expected errors in each method.

-- | AddEditCategoryError - Add and edit category errors
data AddEditCategoryError
  = InvalidPermissionAddEditCategory InvalidAdminPermission
  | -- CategoryAlreadyExisted error when the category with the same name already exists. Duplication of category name is not allowed
    CategoryAlreadyExists InvalidContent
  | -- InvalidParentId when the category parent not exists or parent is its child
    InvalidParentIdAddEditCategory InvalidContentCategoryId
  | AddEditCategorySQLRequestError SQLRequestError
  | -- InvalidCategoryId - use only in edit request
    InvalidCategoryId InvalidId
  | -- | AddEditCategoryUserError - token exists but user not
    AddEditCategorySearchUserError SearchUserError
  | AddEditCategorySomeException EXS.SomeException
  deriving (Show, EXS.Exception)

-- | AddImageError  - Add image error
data AddImageError
  = -- | InvalidPermissionAddImage - the image is not added by the author
    InvalidPermissionAddImage InvalidAuthorPermission
  | -- | NotPngImage - the image is not {"format":"png"}
    NotPngImage InvalidContent
  | -- | NotBase64Image - the image is not Base64 coding
    NotBase64Image InvalidContent
  | -- | ImageFileNotExists - does not exist file (No such file or directory) in request e.x {"image": "/Users/admin/news/_image/white_base64" }
    ImageFileNotExists InvalidContent
  | -- | AddImageSearchUserError - token exists but user not
    AddImageSearchUserError SearchUserError
  | AddImageSQLRequestError SQLRequestError
  | AddImageSomeException EXS.SomeException
  deriving (Show, EXS.Exception)

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
  | -- | ImageFileAddEditNewsNotExists - does not exist file (No such file or directory) in request e.x {"image": "/Users/admin/news/_image/white_base64" }
    ImageFileAddEditNewsNotExists InvalidContent
  | AddEditNewsSQLRequestError SQLRequestError
  | -- | AddUserSearchUserError - token exists but user not
    AddEditNewsSearchUserError SearchUserError
  | -- | InvalidNewsId - use only in edit request
    InvalidNewsId InvalidId
  | AddEditNewsSomeException EXS.SomeException
  deriving (Show, EXS.Exception)

-- | AddUserError - creation user errors
data AddUserError
  = -- |  InvalidPermissionAddUser - the user is not  admin
    InvalidPermissionAddUser InvalidAdminPermission
  | -- |  UserAlreadyExisted - this login already exists
    UserAlreadyExists InvalidContent
  | -- | AddUserSearchUserError - token exists but user not
    AddUserSearchUserError SearchUserError
  | AddUserSQLRequestError SQLRequestError
  | AddUserSomeException EXS.SomeException
  deriving (Show, EXS.Exception)

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

-- | GetContentError - get users or category list error
data GetContentError
  = InvalidOffsetOrLimitGetContent InvalidOffsetOrLimit
  | GetContentSQLRequestError SQLRequestError
  | GetContentSomeException EXS.SomeException
  deriving (Show, EXS.Exception)

-- | GetImageError - get one image
data GetImageError
  = InvalidImagedId InvalidId
  | GetImageSQLRequestError SQLRequestError
  | GetImageSomeException EXS.SomeException
  deriving (Show, EXS.Exception)

-- | SearchUserError - token exists but user does not exist in data base
data SearchUserError
  = SearchUserNotExist SQLRequestError
  | SearchUserSQLRequestError SQLRequestError
  | SearchUserSomeException EXS.SomeException
  deriving (Show, EXS.Exception)

-- | InvalidContentCategoryId - when the category is not valid - it does not exist, or it cannot become a parent
data InvalidContentCategoryId
  = InvalidContentCategoryIdError InvalidContent
  | InvalidContentCategoryIdSQLRequestError SQLRequestError
  | InvalidContentCategoryIdSomeException EXS.SomeException
  deriving (Show, EXS.Exception)

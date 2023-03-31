-- | define Errors in EndPoints
module Types.ErrorTypes where

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
  show (InvalidOffset []) = show error400
  show (InvalidOffset a) = show a
  show (InvalidLimit []) = show error400
  show (InvalidLimit a) = show a

-- | 3
-- | InvalidId  - Then resource with this Id in URI (ex.700) don't exists in the Data Base (http://localhost:8080/image/700)
newtype InvalidId
  = InvalidId String
  deriving (Eq)

instance Show InvalidId where
  show (InvalidId []) = show error404
  show (InvalidId a) = show a

-- | 4
-- | InvalidContent  - Then content in request (ex.700a) is invalid. (--data '{ "new_path": "700a", "new_category": "cat " }' \ )
newtype InvalidContent
  = InvalidContent String
  deriving (Eq)

instance Show InvalidContent where
  show (InvalidContent []) = show error400
  show (InvalidContent a) = show a

-- | 5
-- | InvalidAdminPermission  - Then the user does not have admin permission to execute  request.
newtype InvalidAdminPermission
  = InvalidAdminPermission String
  deriving (Eq)

instance Show InvalidAdminPermission where
  show (InvalidAdminPermission []) = show error404
  show (InvalidAdminPermission a) = show a

-- | 6
-- | InvalidAuthorPermission  - Then the user does not have author permission to execute  request.
newtype InvalidAuthorPermission
  = InvalidAuthorPermission String
  deriving (Eq)

instance Show InvalidAuthorPermission where
  show (InvalidAuthorPermission []) = show error403
  show (InvalidAuthorPermission a) = show a

-- | 7
-- | InvalidRequest   - Then error in uri ( no word "text" in request news/search?text = "Ann")
newtype InvalidRequest
  = InvalidRequest String
  deriving (Eq)

instance Show InvalidRequest where
  show (InvalidRequest []) = show error400
  show (InvalidRequest a) = show a

-- | 8
-- | SQLRequestError  - Error of executing SQL request.
newtype SQLRequestError
  = SQLRequestError String
  deriving (Eq)

instance Show SQLRequestError where
  show (SQLRequestError []) = show error500
  show (SQLRequestError a) = show a

-- | 9
-- | InvalidToken  - Invalid token
newtype InvalidToken
  = InvalidToken String
  deriving (Eq)

instance Show InvalidToken where
  show (InvalidToken []) = show error403
  show (InvalidToken a) = show a

-------------------------------------------------------------

-- | LIST OF ERRORS FOR SERVER Authentication
data ServerAuthError
  = ServerAuthErrorInvalidToken InvalidToken
  | ServerAuthErrorSQLRequestError SQLRequestError
  deriving (Show, Eq)

-------------------------------------------------------------

-- | LIST OF ERRORS FOR EACH ENDPOINT
-- This allows you to see the expected errors in each method.
-- | 1
-- | AddEditCategoryError - Add and edit category errors
data AddEditCategoryError
  = InvalidPermissionAddEditCategory InvalidAdminPermission
  | -- InvalidSyntaxPath error in path such as new_path": "700-a"
    InvalidSyntaxPath InvalidContent
  | -- InvalidValuePath error in path such as new_path": "1.12.100" then "1.12.100"  is not exist
    InvalidValuePath InvalidContent
  | AddEditCategorySQLRequestError SQLRequestError
  | -- InvalidCategoryId - use only in edit request
    InvalidCategoryId InvalidId
  deriving (Show, Eq)

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
  | AddImageSQLRequestError SQLRequestError
  deriving (Show, Eq)

-- | 3
-- | AddEditNewsError  - Add or edit news error
data AddEditNewsError
  = -- | InvalidPermissionAddNews - the news is not added by the author
    InvalidPermissionAddEditNews InvalidAuthorPermission
  | -- | Add news to a non-existent category by id
    InvalidCategoryIdAddEditNews InvalidContent
  | -- | NotPngImageAddNews - the image is not {"format":"png"}
    NotPngImageAddEditNews InvalidContent
  | -- | NotBase64ImageAddNews - the image is not Base64 coding {"image": "iVBORw0KGgoAAAANSU..""}
    NotBase64ImageAddEditNews InvalidContent
  | -- | NotExistImageFileAddNews - does not exist file (No such file or directory) in request e.x {"image": "/Users/admin/news/_image/white_base64" }
    NotExistImageFileAddEditNews InvalidContent
  | AddEditNewsSQLRequestError SQLRequestError
  | -- InvalidNewsId - use only in edit request
    InvalidNewsId InvalidId
  deriving (Show, Eq)

-- | 4
-- | AddUserError - creation user errors
data AddUserError
  = -- |  InvalidPermissionAddUser - the user is not  admin
    InvalidPermissionAddUser InvalidAdminPermission
  | -- |  UserAlreadyExisted - this login already exists
    UserAlreadyExisted InvalidContent
  | AddUserSQLRequestError SQLRequestError
  deriving (Show, Eq)

-- | 5
-- | GetNewsError - get news list error
data GetNewsError
  = InvalidOffsetOrLimitGetNews InvalidOffsetOrLimit
  | -- | InvalidFilterGetNews - only in queries with filters
    InvalidFilterGetNews InvalidRequest
  | -- | InvalidSearchGetNews  - only in search queries. Then error in uri ( no word "text" in request news/search?offset=2&limit=3)
    InvalidSearchGetNews InvalidRequest
  | -- | InvalidPermissionGetNews - only for requests with authentication
    InvalidPermissionGetNews InvalidAuthorPermission
  | GetNewsSQLRequestError SQLRequestError
  deriving (Show, Eq)

-- | 6
-- | GetContentError - get users or category list error
data GetContentError
  = InvalidOffsetOrLimitGetContent InvalidOffsetOrLimit
  | GetContentSQLRequestError SQLRequestError
  deriving (Show, Eq)

-- | 7
-- | GetImageError - get one image
data GetImageError
  = InvalidImagedId InvalidId
  | GetImageSQLRequestError SQLRequestError
  deriving (Show, Eq)

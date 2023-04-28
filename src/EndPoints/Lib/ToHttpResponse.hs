module EndPoints.Lib.ToHttpResponse
  ( toHttpResponse,
  )
where

import qualified Data.ByteString as B
import Servant
  ( Handler,
    ServerError (errReasonPhrase),
    err400,
    err403,
    err404,
    err500,
    throwError,
  )
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

class ToHttpResponse e r where
  toHttpResponse :: Either e r -> Handler r

-- | AddEditCategoryError - Add and edit category errors
instance ToHttpResponse ErrorTypes.AddEditCategoryError DataTypes.Category where
  toHttpResponse (Right category) = return category
  toHttpResponse (Left (ErrorTypes.AddEditCategorySQLRequestError a)) =
    throwError err500 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.InvalidPermissionAddEditCategory a)) =
    throwError err404 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.CategoryAlreadyExisted a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.InvalidParentIdAddEditCategory a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.InvalidCategoryId a)) =
    throwError err404 {errReasonPhrase = show a}

-- | AddImageError  - Add image error
instance ToHttpResponse ErrorTypes.AddImageError DataTypes.URI where
  toHttpResponse (Right uri) = return uri
  toHttpResponse (Left (ErrorTypes.InvalidPermissionAddImage a)) =
    throwError err403 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.AddImageSQLRequestError a)) =
    throwError err500 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.NotPngImage a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.NotBase64Image a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.NotExistImageFile a)) =
    throwError err400 {errReasonPhrase = show a}

-- | AddEditNewsError  - Add or edit news error
instance ToHttpResponse ErrorTypes.AddEditNewsError DataTypes.News where
  toHttpResponse (Right news) = return news
  toHttpResponse (Left (ErrorTypes.AddEditNewsSQLRequestError a)) =
    throwError err500 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.InvalidPermissionAddEditNews a)) =
    throwError err403 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.InvalidCategoryIdAddEditNews a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.NotPngImageAddEditNews a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.NotBase64ImageAddEditNews a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.NotExistImageFileAddEditNews a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.InvalidNewsId a)) =
    throwError err404 {errReasonPhrase = show a}

-- | AddUserError - creation user errors
instance ToHttpResponse ErrorTypes.AddUserError DataTypes.User where
  toHttpResponse (Right user) = return user
  toHttpResponse (Left (ErrorTypes.InvalidPermissionAddUser a)) =
    throwError err404 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.UserAlreadyExisted a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.AddUserSQLRequestError a)) =
    throwError err500 {errReasonPhrase = show a}

-- | GetNewsError - get news list error
instance ToHttpResponse ErrorTypes.GetNewsError [DataTypes.News] where
  toHttpResponse (Right news) = return news
  toHttpResponse (Left (ErrorTypes.GetNewsSQLRequestError a)) =
    throwError err500 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.InvalidPermissionGetNews a)) =
    throwError err403 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.InvalidOffsetOrLimitGetNews a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.InvalidFilterGetNews a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.InvalidSearchGetNews a)) =
    throwError err400 {errReasonPhrase = show a}

-- | GetContentError - get users or category list error
instance ToHttpResponse ErrorTypes.GetContentError [DataTypes.Category] where
  toHttpResponse (Right cat) = return cat
  toHttpResponse (Left (ErrorTypes.GetContentSQLRequestError a)) =
    throwError err500 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.InvalidOffsetOrLimitGetContent a)) =
    throwError err400 {errReasonPhrase = show a}

instance ToHttpResponse ErrorTypes.GetContentError [DataTypes.User] where
  toHttpResponse (Right cat) = return cat
  toHttpResponse (Left (ErrorTypes.GetContentSQLRequestError a)) =
    throwError err500 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.InvalidOffsetOrLimitGetContent a)) =
    throwError err400 {errReasonPhrase = show a}

-- | GetImageError - get one image
instance ToHttpResponse ErrorTypes.GetImageError B.ByteString where
  toHttpResponse (Right str) = return str
  toHttpResponse (Left (ErrorTypes.GetImageSQLRequestError a)) =
    throwError err500 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.InvalidImagedId a)) =
    throwError err404 {errReasonPhrase = show a}

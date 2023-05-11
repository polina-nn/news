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
  toHttpResponse (Left (ErrorTypes.AddEditCategorySQLRequestError _)) =
    throwError err500 {errReasonPhrase = ErrorTypes.error500}
  toHttpResponse (Left (ErrorTypes.InvalidPermissionAddEditCategory _)) =
    throwError err404 {errReasonPhrase = ErrorTypes.error404}
  toHttpResponse (Left (ErrorTypes.CategoryAlreadyExists a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.InvalidParentIdAddEditCategory a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.InvalidCategoryId _)) =
    throwError err404 {errReasonPhrase = ErrorTypes.error404}
  toHttpResponse (Left (ErrorTypes.AddEditCategorySearchUserError _)) =
    throwError err500 {errReasonPhrase = ErrorTypes.error500}
  toHttpResponse (Left (ErrorTypes.AddEditCategorySomeException _)) =
    throwError err500 {errReasonPhrase = ErrorTypes.error500}

-- | AddImageError  - Add image error
instance ToHttpResponse ErrorTypes.AddImageError DataTypes.URI where
  toHttpResponse (Right uri) = return uri
  toHttpResponse (Left (ErrorTypes.InvalidPermissionAddImage _)) =
    throwError err403 {errReasonPhrase = ErrorTypes.error403}
  toHttpResponse (Left (ErrorTypes.AddImageSQLRequestError _)) =
    throwError err500 {errReasonPhrase = ErrorTypes.error500}
  toHttpResponse (Left (ErrorTypes.NotPngImage a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.NotBase64Image a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.ImageFileNotExists a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.AddImageSearchUserError _)) =
    throwError err500 {errReasonPhrase = ErrorTypes.error500}
  toHttpResponse (Left (ErrorTypes.AddImageSomeException _)) =
    throwError err500 {errReasonPhrase = ErrorTypes.error500}

-- | AddEditNewsError  - Add or edit news error
instance ToHttpResponse ErrorTypes.AddEditNewsError DataTypes.News where
  toHttpResponse (Right news) = return news
  toHttpResponse (Left (ErrorTypes.AddEditNewsSQLRequestError _)) =
    throwError err500 {errReasonPhrase = ErrorTypes.error500}
  toHttpResponse (Left (ErrorTypes.InvalidPermissionAddEditNews _)) =
    throwError err403 {errReasonPhrase = ErrorTypes.error403}
  toHttpResponse (Left (ErrorTypes.InvalidCategoryIdAddEditNews a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.NotPngImageAddEditNews a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.NotBase64ImageAddEditNews a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.ImageFileAddEditNewsNotExists a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.InvalidNewsId _)) =
    throwError err404 {errReasonPhrase = ErrorTypes.error404}
  toHttpResponse (Left (ErrorTypes.AddEditNewsSearchUserError _)) =
    throwError err500 {errReasonPhrase = ErrorTypes.error500}
  toHttpResponse (Left (ErrorTypes.AddEditNewsSomeException _)) =
    throwError err500 {errReasonPhrase = ErrorTypes.error500}

-- | AddUserError - creation user errors
instance ToHttpResponse ErrorTypes.AddUserError DataTypes.User where
  toHttpResponse (Right user) = return user
  toHttpResponse (Left (ErrorTypes.InvalidPermissionAddUser _)) =
    throwError err404 {errReasonPhrase = ErrorTypes.error404}
  toHttpResponse (Left (ErrorTypes.UserAlreadyExists a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.AddUserSQLRequestError _)) =
    throwError err500 {errReasonPhrase = ErrorTypes.error500}
  toHttpResponse (Left (ErrorTypes.AddUserSearchUserError _)) =
    throwError err500 {errReasonPhrase = ErrorTypes.error500}
  toHttpResponse (Left (ErrorTypes.AddUserSomeException _)) =
    throwError err500 {errReasonPhrase = ErrorTypes.error500}

-- | GetNewsError - get news list error
instance ToHttpResponse ErrorTypes.GetNewsError [DataTypes.News] where
  toHttpResponse (Right news) = return news
  toHttpResponse (Left (ErrorTypes.GetNewsSQLRequestError _)) =
    throwError err500 {errReasonPhrase = ErrorTypes.error500}
  toHttpResponse (Left (ErrorTypes.InvalidPermissionGetNews _)) =
    throwError err403 {errReasonPhrase = ErrorTypes.error403}
  toHttpResponse (Left (ErrorTypes.InvalidOffsetOrLimitGetNews a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.InvalidFilterGetNews a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.InvalidSearchGetNews a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.GetNewsSearchUserError _)) =
    throwError err500 {errReasonPhrase = ErrorTypes.error500}
  toHttpResponse (Left (ErrorTypes.GetNewsSomeException _)) =
    throwError err500 {errReasonPhrase = ErrorTypes.error500}
  toHttpResponse (Left (ErrorTypes.InvalidCategoryIdGetNews a)) =
    throwError err400 {errReasonPhrase = show a}
    
-- | GetContentError - get users or category list error
instance ToHttpResponse ErrorTypes.GetContentError a where
  toHttpResponse (Right cat) = return cat
  toHttpResponse (Left (ErrorTypes.GetContentSQLRequestError _)) =
    throwError err500 {errReasonPhrase = ErrorTypes.error500}
  toHttpResponse (Left (ErrorTypes.InvalidOffsetOrLimitGetContent a)) =
    throwError err400 {errReasonPhrase = show a}
  toHttpResponse (Left (ErrorTypes.GetContentSomeException _)) =
    throwError err500 {errReasonPhrase = ErrorTypes.error500}

-- | GetImageError - get one image
instance ToHttpResponse ErrorTypes.GetImageError B.ByteString where
  toHttpResponse (Right str) = return str
  toHttpResponse (Left (ErrorTypes.GetImageSQLRequestError _)) =
    throwError err500 {errReasonPhrase = ErrorTypes.error500}
  toHttpResponse (Left (ErrorTypes.InvalidImagedId _)) =
    throwError err404 {errReasonPhrase = ErrorTypes.error404}
  toHttpResponse (Left (ErrorTypes.GetImageSomeException _)) =
    throwError err500 {errReasonPhrase = ErrorTypes.error500}

module EndPoints.Lib.ThrowError
  ( throwSqlRequestError,
    throwSomeException,
    throwInvalidId,
    throwInvalidSearch,
    throwInvalidFilter,
    throwInvalidAdminPermission,
    throwInvalidAuthorPermission,
    throwAlreadyExists,
    throwNotExists,
    throwNotBase64,
    throwNotPng,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.Text as T
import Logger (logError, (.<))
import qualified News
import qualified Types.ErrorTypes as ErrorTypes

type ErrorFunction = T.Text

------ ThrowSomeException
class ThrowSomeException a b where
  throwSomeException :: Monad m => News.Handle m -> ErrorFunction -> EXS.SomeException -> EX.ExceptT a m b

throwSomeException' :: Monad m => News.Handle m -> ErrorFunction -> EXS.SomeException -> EX.ExceptT EXS.SomeException m b
throwSomeException' h f e = do
  lift $ Logger.logError (News.hLogHandle h) $ "SOME EXCEPTION " .< f <> " : " .< e
  EX.throwE $ EXS.SomeException e

instance ThrowSomeException ErrorTypes.ServerAuthError b where
  throwSomeException h f e = EX.withExceptT ErrorTypes.ServerAuthSomeException $ throwSomeException' h f e

instance ThrowSomeException ErrorTypes.AddEditCategoryError b where
  throwSomeException h f e = EX.withExceptT ErrorTypes.AddEditCategorySomeException $ throwSomeException' h f e

instance ThrowSomeException ErrorTypes.AddImageError b where
  throwSomeException h f e = EX.withExceptT ErrorTypes.AddImageSomeException $ throwSomeException' h f e

instance ThrowSomeException ErrorTypes.AddEditNewsError b where
  throwSomeException h f e = EX.withExceptT ErrorTypes.AddEditNewsSomeException $ throwSomeException' h f e

instance ThrowSomeException ErrorTypes.AddUserError b where
  throwSomeException h f e = EX.withExceptT ErrorTypes.AddUserSomeException $ throwSomeException' h f e

instance ThrowSomeException ErrorTypes.GetNewsError b where
  throwSomeException h f e = EX.withExceptT ErrorTypes.GetNewsSomeException $ throwSomeException' h f e

instance ThrowSomeException ErrorTypes.GetContentError b where
  throwSomeException h f e = EX.withExceptT ErrorTypes.GetContentSomeException $ throwSomeException' h f e

instance ThrowSomeException ErrorTypes.GetImageError b where
  throwSomeException h f e = EX.withExceptT ErrorTypes.GetImageSomeException $ throwSomeException' h f e

instance ThrowSomeException ErrorTypes.SearchUserError b where
  throwSomeException h f e = EX.withExceptT ErrorTypes.SearchUserSomeException $ throwSomeException' h f e

instance ThrowSomeException ErrorTypes.InvalidContentCategoryId b where
  throwSomeException h f e = EX.withExceptT ErrorTypes.InvalidContentCategoryIdSomeException $ throwSomeException' h f e

------ ThrowSqlRequestError
class ThrowSqlRequestError a b where
  throwSqlRequestError :: Monad m => News.Handle m -> ErrorFunction -> ErrorTypes.SQLRequestError -> EX.ExceptT a m b

throwSqlRequestError' :: Monad m => News.Handle m -> ErrorFunction -> ErrorTypes.SQLRequestError -> EX.ExceptT ErrorTypes.SQLRequestError m b
throwSqlRequestError' h f e = do
  lift $ Logger.logError (News.hLogHandle h) $ "ERROR ErrorTypes.SQLRequestError " .< f <> " : " .< e
  EX.throwE e

instance ThrowSqlRequestError ErrorTypes.ServerAuthError b where
  throwSqlRequestError h f e = EX.withExceptT ErrorTypes.ServerAuthErrorSQLRequestError $ throwSqlRequestError' h f e

instance ThrowSqlRequestError ErrorTypes.AddEditNewsError b where
  throwSqlRequestError h f e = EX.withExceptT ErrorTypes.AddEditNewsSQLRequestError $ throwSqlRequestError' h f e

instance ThrowSqlRequestError ErrorTypes.AddEditCategoryError b where
  throwSqlRequestError h f e = EX.withExceptT ErrorTypes.AddEditCategorySQLRequestError $ throwSqlRequestError' h f e

instance ThrowSqlRequestError ErrorTypes.AddImageError b where
  throwSqlRequestError h f e = EX.withExceptT ErrorTypes.AddImageSQLRequestError $ throwSqlRequestError' h f e

instance ThrowSqlRequestError ErrorTypes.AddUserError b where
  throwSqlRequestError h f e = EX.withExceptT ErrorTypes.AddUserSQLRequestError $ throwSqlRequestError' h f e

instance ThrowSqlRequestError ErrorTypes.GetImageError b where
  throwSqlRequestError h f e = EX.withExceptT ErrorTypes.GetImageSQLRequestError $ throwSqlRequestError' h f e

instance ThrowSqlRequestError ErrorTypes.GetContentError b where
  throwSqlRequestError h f e = EX.withExceptT ErrorTypes.GetContentSQLRequestError $ throwSqlRequestError' h f e

instance ThrowSqlRequestError ErrorTypes.GetNewsError b where
  throwSqlRequestError h f e = EX.withExceptT ErrorTypes.GetNewsSQLRequestError $ throwSqlRequestError' h f e

instance ThrowSqlRequestError ErrorTypes.SearchUserError b where
  throwSqlRequestError h f e = EX.withExceptT ErrorTypes.SearchUserSQLRequestError $ throwSqlRequestError' h f e

instance ThrowSqlRequestError ErrorTypes.InvalidContentCategoryId b where
  throwSqlRequestError h f e = EX.withExceptT ErrorTypes.InvalidContentCategoryIdSQLRequestError $ throwSqlRequestError' h f e

------ ThrowInvalidId
class ThrowInvalidId a b where
  throwInvalidId :: Monad m => News.Handle m -> ErrorFunction -> ErrorTypes.InvalidId -> EX.ExceptT a m b

throwInvalidId' :: Monad m => News.Handle m -> ErrorFunction -> ErrorTypes.InvalidId -> EX.ExceptT ErrorTypes.InvalidId m b
throwInvalidId' h f e = do
  lift $ Logger.logError (News.hLogHandle h) $ "ERROR ErrorTypes.InvalidId " .< f <> " : " .< e
  EX.throwE e

instance ThrowInvalidId ErrorTypes.AddEditCategoryError b where
  throwInvalidId h f e = EX.withExceptT ErrorTypes.InvalidCategoryId $ throwInvalidId' h f e

instance ThrowInvalidId ErrorTypes.AddEditNewsError b where
  throwInvalidId h f e = EX.withExceptT ErrorTypes.InvalidNewsId $ throwInvalidId' h f e

instance ThrowInvalidId ErrorTypes.GetImageError b where
  throwInvalidId h f e = EX.withExceptT ErrorTypes.InvalidImagedId $ throwInvalidId' h f e

------  ThrowInvalidSearch
class ThrowInvalidSearch a b where
  throwInvalidSearch :: Monad m => News.Handle m -> ErrorFunction -> ErrorTypes.InvalidRequest -> EX.ExceptT a m b

throwInvalidSearch' :: Monad m => News.Handle m -> ErrorFunction -> ErrorTypes.InvalidRequest -> EX.ExceptT ErrorTypes.InvalidRequest m b
throwInvalidSearch' h f e = do
  lift $ Logger.logError (News.hLogHandle h) $ "ERROR InvalidSearch:  ErrorTypes.InvalidRequest " .< f <> " : " .< e
  EX.throwE e

instance ThrowInvalidSearch ErrorTypes.GetNewsError b where
  throwInvalidSearch h f e = EX.withExceptT ErrorTypes.InvalidSearchGetNews $ throwInvalidSearch' h f e

------ ThrowInvalidFilter
class ThrowInvalidFilter a b where
  throwInvalidFilter :: Monad m => News.Handle m -> ErrorFunction -> ErrorTypes.InvalidRequest -> EX.ExceptT a m b

throwInvalidFilter' :: Monad m => News.Handle m -> ErrorFunction -> ErrorTypes.InvalidRequest -> EX.ExceptT ErrorTypes.InvalidRequest m b
throwInvalidFilter' h f e = do
  lift $ Logger.logError (News.hLogHandle h) $ "ERROR InvalidFilter: ErrorTypes.InvalidRequest " .< f <> " : " .< e
  EX.throwE e

instance ThrowInvalidFilter ErrorTypes.GetNewsError b where
  throwInvalidFilter h f e = EX.withExceptT ErrorTypes.InvalidFilterGetNews $ throwInvalidFilter' h f e

------ ThrowInvalidAuthorPermission
class ThrowInvalidAuthorPermission a b where
  throwInvalidAuthorPermission :: Monad m => News.Handle m -> ErrorFunction -> ErrorTypes.InvalidAuthorPermission -> EX.ExceptT a m b

throwInvalidAuthorPermission' :: Monad m => News.Handle m -> ErrorFunction -> ErrorTypes.InvalidAuthorPermission -> EX.ExceptT ErrorTypes.InvalidAuthorPermission m b
throwInvalidAuthorPermission' h f e = do
  lift $ Logger.logError (News.hLogHandle h) $ "ERROR  ErrorTypes.InvalidAuthorPermission " .< f <> " : " .< e
  EX.throwE e

instance ThrowInvalidAuthorPermission ErrorTypes.InvalidAuthorPermission b where
  throwInvalidAuthorPermission = throwInvalidAuthorPermission'

instance ThrowInvalidAuthorPermission ErrorTypes.AddEditNewsError b where
  throwInvalidAuthorPermission h f e = EX.withExceptT ErrorTypes.InvalidPermissionAddEditNews $ throwInvalidAuthorPermission' h f e

------ ThrowInvalidAdminPermission
class ThrowInvalidAdminPermission a b where
  throwInvalidAdminPermission :: Monad m => News.Handle m -> ErrorFunction -> ErrorTypes.InvalidAdminPermission -> EX.ExceptT a m b

throwInvalidAdminPermission' :: Monad m => News.Handle m -> ErrorFunction -> ErrorTypes.InvalidAdminPermission -> EX.ExceptT ErrorTypes.InvalidAdminPermission m b
throwInvalidAdminPermission' h f e = do
  lift $ Logger.logError (News.hLogHandle h) $ "ERROR  ErrorTypes.InvalidAdminPermission " .< f <> " : " .< e
  EX.throwE e

instance ThrowInvalidAdminPermission ErrorTypes.InvalidAdminPermission b where
  throwInvalidAdminPermission = throwInvalidAdminPermission'

------ ThrowNotExists
class ThrowNotExists a b where
  throwNotExists :: Monad m => News.Handle m -> ErrorFunction -> ErrorTypes.InvalidContent -> EX.ExceptT a m b

throwNotExists' :: Monad m => News.Handle m -> ErrorFunction -> ErrorTypes.InvalidContent -> EX.ExceptT ErrorTypes.InvalidContent m b
throwNotExists' h f e = do
  lift $ Logger.logError (News.hLogHandle h) $ "ERROR  ErrorTypes.InvalidContent " .< f <> " : " .< e
  EX.throwE e

instance ThrowNotExists ErrorTypes.AddEditNewsError b where
  throwNotExists h f e = EX.withExceptT ErrorTypes.ImageFileAddEditNewsNotExists $ throwNotExists' h f e

instance ThrowNotExists ErrorTypes.AddImageError b where
  throwNotExists h f e = EX.withExceptT ErrorTypes.ImageFileNotExists $ throwNotExists' h f e

instance ThrowNotExists ErrorTypes.InvalidContentCategoryId b where
  throwNotExists h f e = EX.withExceptT ErrorTypes.InvalidContentCategoryIdError $ throwNotExists' h f e

------ ThrowAlreadyExists
class ThrowAlreadyExists a b where
  throwAlreadyExists :: Monad m => News.Handle m -> ErrorFunction -> ErrorTypes.InvalidContent -> EX.ExceptT a m b

throwAlreadyExists' :: Monad m => News.Handle m -> ErrorFunction -> ErrorTypes.InvalidContent -> EX.ExceptT ErrorTypes.InvalidContent m b
throwAlreadyExists' h f e = do
  lift $ Logger.logError (News.hLogHandle h) $ "ERROR  ErrorTypes.InvalidContent " .< f <> " : " .< e
  EX.throwE e

instance ThrowAlreadyExists ErrorTypes.AddEditCategoryError b where
  throwAlreadyExists h f e = EX.withExceptT ErrorTypes.CategoryAlreadyExists $ throwAlreadyExists' h f e

instance ThrowAlreadyExists ErrorTypes.AddUserError b where
  throwAlreadyExists h f e = EX.withExceptT ErrorTypes.UserAlreadyExists $ throwAlreadyExists' h f e

------ ThrowNotBase64
class ThrowNotBase64 a b where
  throwNotBase64 :: Monad m => News.Handle m -> ErrorFunction -> ErrorTypes.InvalidContent -> EX.ExceptT a m b

throwNotBase64' :: Monad m => News.Handle m -> ErrorFunction -> ErrorTypes.InvalidContent -> EX.ExceptT ErrorTypes.InvalidContent m b
throwNotBase64' h f e = do
  lift $ Logger.logError (News.hLogHandle h) $ "ERROR  ErrorTypes.InvalidContent " .< f <> " : " .< e
  EX.throwE e

instance ThrowNotBase64 ErrorTypes.AddEditNewsError b where
  throwNotBase64 h f e = EX.withExceptT ErrorTypes.NotBase64ImageAddEditNews $ throwNotBase64' h f e

instance ThrowNotBase64 ErrorTypes.AddImageError b where
  throwNotBase64 h f e = EX.withExceptT ErrorTypes.NotBase64Image $ throwNotBase64' h f e

------ ThrowNotPng
class ThrowNotPng a b where
  throwNotPng :: Monad m => News.Handle m -> ErrorFunction -> ErrorTypes.InvalidContent -> EX.ExceptT a m b

throwNotPng' :: Monad m => News.Handle m -> ErrorFunction -> ErrorTypes.InvalidContent -> EX.ExceptT ErrorTypes.InvalidContent m b
throwNotPng' h f e = do
  lift $ Logger.logError (News.hLogHandle h) $ "ERROR  ErrorTypes.InvalidContent " .< f <> " : " .< e
  EX.throwE e

instance ThrowNotPng ErrorTypes.AddEditNewsError b where
  throwNotPng h f e = EX.withExceptT ErrorTypes.NotBase64ImageAddEditNews $ throwNotPng' h f e

instance ThrowNotPng ErrorTypes.AddImageError b where
  throwNotPng h f e = EX.withExceptT ErrorTypes.NotBase64Image $ throwNotPng' h f e

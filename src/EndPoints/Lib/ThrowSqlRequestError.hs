module EndPoints.Lib.ThrowSqlRequestError
  ( throwSqlRequestError,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import Logger (logError, (.<))
import qualified News
import qualified Types.ErrorTypes as ErrorTypes

-- | ErrorDescription - error description: function, text
type ErrorDescription = (String, String)

class ThrowSqlRequestError a b where
  throwSqlRequestError :: News.Handle IO -> ErrorDescription -> EX.ExceptT a IO b

throwSqlRequestError' :: MonadIO m => News.Handle IO -> ErrorDescription -> EX.ExceptT ErrorTypes.SQLRequestError m b
throwSqlRequestError' h (f, e) = do
  liftIO $
    Logger.logError
      (News.hLogHandle h)
      ("ERROR " .< ErrorTypes.SQLRequestError (f <> ": BAD! " <> e))
  EX.throwE $ ErrorTypes.SQLRequestError []

instance ThrowSqlRequestError ErrorTypes.SQLRequestError b where
  throwSqlRequestError = throwSqlRequestError'

instance ThrowSqlRequestError ErrorTypes.ServerAuthError b where
  throwSqlRequestError h (f, e) = EX.withExceptT ErrorTypes.ServerAuthErrorSQLRequestError $ throwSqlRequestError' h (f, e)

instance ThrowSqlRequestError ErrorTypes.AddEditNewsError b where
  throwSqlRequestError h (f, e) = EX.withExceptT ErrorTypes.AddEditNewsSQLRequestError $ throwSqlRequestError' h (f, e)

instance ThrowSqlRequestError ErrorTypes.AddEditCategoryError b where
  throwSqlRequestError h (f, e) = EX.withExceptT ErrorTypes.AddEditCategorySQLRequestError $ throwSqlRequestError' h (f, e)

instance ThrowSqlRequestError ErrorTypes.AddImageError b where
  throwSqlRequestError h (f, e) = EX.withExceptT ErrorTypes.AddImageSQLRequestError $ throwSqlRequestError' h (f, e)

instance ThrowSqlRequestError ErrorTypes.AddUserError b where
  throwSqlRequestError h (f, e) = EX.withExceptT ErrorTypes.AddUserSQLRequestError $ throwSqlRequestError' h (f, e)

instance ThrowSqlRequestError ErrorTypes.GetImageError b where
  throwSqlRequestError h (f, e) = EX.withExceptT ErrorTypes.GetImageSQLRequestError $ throwSqlRequestError' h (f, e)

instance ThrowSqlRequestError ErrorTypes.GetContentError b where
  throwSqlRequestError h (f, e) = EX.withExceptT ErrorTypes.GetContentSQLRequestError $ throwSqlRequestError' h (f, e)

instance ThrowSqlRequestError ErrorTypes.GetNewsError b where
  throwSqlRequestError h (f, e) = EX.withExceptT ErrorTypes.GetNewsSQLRequestError $ throwSqlRequestError' h (f, e)

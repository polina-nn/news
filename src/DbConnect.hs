module DbConnect
  ( tryRequestConnectDb,
    tryInitConnectDb,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import qualified Database.PostgreSQL.Simple as SQL
import Logger (logDebug, logError)
import qualified News
import qualified Types.ErrorTypes as ErrorTypes
import qualified Types.ExceptionTypes as ExceptionTypes

-- | tryInitConnectDb  - throws an exception if it was not possible to connect to the database, use in DbServices.initConnPool.
tryInitConnectDb :: News.Handle IO -> IO SQL.Connection
tryInitConnectDb h = do
  res <- EXS.try (SQL.connect (url h))
  case res of
    Left e -> do
      Logger.logError (News.hLogHandle h) "tryInitConnectDb: BAD, not connection to the Data Base"
      EXS.throw (ExceptionTypes.DbNotConnect e)
    Right conn -> do
      Logger.logDebug (News.hLogHandle h) "tryInitConnectDb: OK "
      pure conn

-- | tryRequestConnectDb - Use it in each endpoints (before request), if the connection to the database failed, I send an SQLRequestError
tryRequestConnectDb :: News.Handle IO -> EX.ExceptT ErrorTypes.SQLRequestError IO SQL.Connection
tryRequestConnectDb h = do
  try <- liftIO tryConnect
  case try of
    Left _ -> do
      liftIO $ Logger.logError (News.hLogHandle h) "tryConnectDataBase: BAD, not connection to Data Base"
      EX.throwE $ ErrorTypes.SQLRequestError []
    Right conn -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) "tryConnectDataBase: OK "
      return conn
  where
    tryConnect :: IO (Either EXS.SomeException SQL.Connection)
    tryConnect = EXS.try (SQL.connect (url h))

url :: News.Handle IO -> SQL.ConnectInfo
url h =
  SQL.ConnectInfo
    { connectHost = News.dbHost (News.hDbConfig h),
      connectPort = read $ News.dbPort (News.hDbConfig h),
      connectUser = News.user (News.hDbConfig h),
      connectPassword = News.password (News.hDbConfig h),
      connectDatabase = News.dbName (News.hDbConfig h)
    }

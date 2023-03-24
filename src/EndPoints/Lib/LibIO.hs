-- |  EndPoints.Lib.Lib - library of helper IO functions for EndPoints and server.hs
module EndPoints.Lib.LibIO
  ( searchUser,
    searchAccount,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import Data.ByteString (ByteString)
import qualified Data.Pool as POOL
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified DbConnect
import qualified EndPoints.Lib.Lib as Lib
import Logger (logDebug, logError, (.<))
import qualified News
import Types.DataTypes (Account (unAccount))
import qualified Types.DataTypes as DataType
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes
import qualified Types.ExceptionTypes as ExceptionTypes

-- | searchUser - searchUser by account in database. Account  equals login, logins are not repeated in the database
searchUser ::
  News.Handle IO ->
  SQL.Connection ->
  DataType.Account ->
  EX.ExceptT ErrorTypes.SQLRequestError IO DataTypes.User
searchUser h conn a@DataTypes.Account {..} = do
  liftIO $ Logger.logDebug (News.hLogHandle h) $ T.concat ["searchUser: ", T.pack $ show a]
  liftIO $ print "searchUser"
  res <-
    liftIO $
      SQL.query
        conn
        [sql|SELECT usr_name, usr_login, usr_admin, usr_author, usr_created 
               FROM usr usr WHERE usr_login= ? |]
        (SQL.Only unAccount)
  case res of
    [user] -> return $ Lib.toUser user
    _ -> do
      liftIO $
        Logger.logError
          (News.hLogHandle h)
          ( "ERROR "
              .< ErrorTypes.SQLRequestError
                "searchUser: BAD! Developer error!"
          )
      EX.throwE $ ErrorTypes.SQLRequestError []

-- | searchUser - searchUser by account in database. Account  equals login, logins are not repeated in the database, but I check it in  countAccounts
searchAccount :: SQL.Connection -> (News.Handle IO, ByteString) -> EX.ExceptT ErrorTypes.ServerAuthError IO DataTypes.Account
searchAccount conn (h, key) = do
  -- conn <- EX.withExceptT ErrorTypes.ServerAuthErrorSQLRequestError $ DbConnect.tryRequestConnectDb h
  _ <- liftIO $ print "searchAccount"
  -- _ <- countAccounts h conn key
  res <-
    liftIO $
      SQL.query
        conn
        [sql| SELECT  cookie_account FROM cookie WHERE cookie_key = ? |]
        (SQL.Only key)
  case res of
    [SQL.Only m] -> return $ toAccount m
    _ -> EX.throwE $ ErrorTypes.ServerAuthErrorSQLRequestError $ ErrorTypes.SQLRequestError []
  where
    toAccount :: T.Text -> DataTypes.Account
    toAccount unAccount = DataTypes.Account {..}

--}

{--
searchAccount :: POOL.Pool SQL.Connection -> (News.Handle IO, ByteString) -> EX.ExceptT ErrorTypes.ServerAuthError IO DataTypes.Account
searchAccount pool (h, key) = do
  -- conn <- EX.withExceptT ErrorTypes.ServerAuthErrorSQLRequestError $ DbConnect.tryRequestConnectDb h
  _ <- liftIO $ print "searchAccount"
  -- _ <- countAccounts h conn key
  maybeConn <- liftIO $ POOL.tryTakeResource pool
  case maybeConn of
    Nothing -> do
      liftIO $ Logger.logError (News.hLogHandle h) "searchAccount: BAD! error500 "
      EX.throwE $ ErrorTypes.ServerAuthErrorSQLRequestError $ ErrorTypes.SQLRequestError []
    Just (conn, localPool) -> do
      res <-
        liftIO
          ( EXS.try $
              SQL.query
                conn
                [sql| SELECT  cookie_account FROM cookie WHERE cookie_key = ? |]
                (SQL.Only key) ::
              IO (Either SQL.SqlError [SQL.Only T.Text])
          )

      case res of
        Left _ -> do
          liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidCookie "searchAccount: BAD! Connection disconnected")
          --  _ <- liftIO $ POOL.destroyResource pool localPool conn
          --  _ <- liftIO $ POOL.destroyAllResources pool

          EX.throwE $ ErrorTypes.ServerAuthErrorSQLRequestError $ ErrorTypes.SQLRequestError []
        Right [SQL.Only m] -> do
          return $ toAccount m
        _ -> EX.throwE $ ErrorTypes.ServerAuthErrorSQLRequestError $ ErrorTypes.SQLRequestError []
  where
    toAccount :: T.Text -> DataTypes.Account
    toAccount unAccount = DataTypes.Account {..}
    --}

initConnPool :: DataTypes.Handle -> IO (POOL.Pool SQL.Connection)
initConnPool h = do
  conn <- EXS.catch (DbConnect.tryInitConnectDb (DataTypes.hServerHandle h)) (ExceptionTypes.handleException (DataTypes.hServerHandle h))
  POOL.createPool (pure conn) SQL.close noOfStripes' (realToFrac idleTime') stripeSize'
  where
    noOfStripes' = News.noOfStripes (News.hDbConfig (DataTypes.hServerHandle h))
    idleTime' = News.idleTime (News.hDbConfig (DataTypes.hServerHandle h))
    stripeSize' = News.stripeSize (News.hDbConfig (DataTypes.hServerHandle h))

countAccounts :: News.Handle IO -> SQL.Connection -> ByteString -> EX.ExceptT ErrorTypes.ServerAuthError IO Int
countAccounts h conn key = do
  _ <- liftIO $ print "countAccounts"
  res <-
    liftIO
      ( EXS.try
          ( SQL.query
              conn
              [sql| SELECT COUNT (cookie_account) FROM cookie WHERE cookie_key = ? |]
              (SQL.Only key)
          ) ::
          IO (Either SQL.SqlError [SQL.Only Int])
      )
  _ <- liftIO $ print res
  case res of
    Left _ -> do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidCookie "countAccounts: BAD! Connection disconnected")
      EX.throwE $ ErrorTypes.ServerAuthErrorSQLRequestError $ ErrorTypes.SQLRequestError []
    Right [SQL.Only m] ->
      case m of
        1 -> do
          liftIO $ Logger.logDebug (News.hLogHandle h) "countAccounts: OK!"
          return m
        0 -> do
          liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidCookie "countAccounts: BAD! Invalid Cookie")
          EX.throwE $ ErrorTypes.ServerAuthErrorInvalidCookie $ ErrorTypes.InvalidCookie []
        _ -> do
          liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidCookie "countAccounts: BAD! Developer error! Duplicate Cookie")
          EX.throwE $ ErrorTypes.ServerAuthErrorInvalidCookie $ ErrorTypes.InvalidCookie []
    Right _ -> do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidCookie "countAccounts: BAD! empty result")
      EX.throwE $ ErrorTypes.ServerAuthErrorSQLRequestError $ ErrorTypes.SQLRequestError []

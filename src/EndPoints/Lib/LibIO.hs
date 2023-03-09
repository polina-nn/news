-- |  EndPoints.Lib.Lib - library of helper IO functions for EndPoints and server.hs
module EndPoints.Lib.LibIO
  ( searchUser,
    searchAccount,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import Data.ByteString (ByteString)
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

-- | searchUser - searchUser by account in database. Account  equals login, logins are not repeated in the database
searchUser ::
  News.Handle IO ->
  SQL.Connection ->
  DataType.Account ->
  EX.ExceptT ErrorTypes.SQLRequestError IO DataTypes.User
searchUser h conn a@DataTypes.Account {..} = do
  liftIO $ Logger.logDebug (News.hLogHandle h) $ T.concat ["searchUser: ", T.pack $ show a]
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
searchAccount _ (h, key) = do
  conn <- EX.withExceptT ErrorTypes.ServerAuthErrorSQLRequestError $ DbConnect.tryRequestConnectDb h
  _ <- countAccounts h conn key
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

countAccounts :: News.Handle IO -> SQL.Connection -> ByteString -> EX.ExceptT ErrorTypes.ServerAuthError IO Int
countAccounts h conn key = do
  res <-
    liftIO $
      SQL.query
        conn
        [sql| SELECT COUNT (cookie_account) FROM cookie WHERE cookie_key = ? |]
        (SQL.Only key)
  case res of
    [SQL.Only m] ->
      case m of
        1 -> return m
        0 -> do
          liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidCookie "countAccounts: BAD! Invalid Cookie")
          EX.throwE $ ErrorTypes.ServerAuthErrorInvalidCookie $ ErrorTypes.InvalidCookie []
        _ -> do
          liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidCookie "countAccounts: BAD! Developer error! Duplicate Cookie")
          EX.throwE $ ErrorTypes.ServerAuthErrorInvalidCookie $ ErrorTypes.InvalidCookie []
    _ -> do
      EX.throwE $ ErrorTypes.ServerAuthErrorSQLRequestError $ ErrorTypes.SQLRequestError []

module EndPoints.GetUserList
  ( getUserList,
    userList,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import Data.Function (flip)
import qualified Data.Pool as POOL
import qualified Data.Text as T
import qualified Data.Time as TIME
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified DbConnect
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.OffsetLimit as OffsetLimit
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logDebug, logError, logInfo, (.<))
import qualified News
import Servant (Handler)
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

getUserList ::
  News.Handle IO ->
  DataTypes.Db ->
  Maybe DataTypes.Offset ->
  Maybe DataTypes.Limit ->
  Handler [DataTypes.User]
getUserList h DataTypes.Db {..} ma ml =
  (>>=) (liftIO $ dbUserList (h, ma, ml)) ToHttpResponse.toHttpResponse

userList ::
  DataTypes.StatePool ->
  (News.Handle IO, Maybe DataTypes.Offset, Maybe DataTypes.Limit) ->
  IO (Either ErrorTypes.GetContentError [DataTypes.User])
userList pool (h, mo, ml) = POOL.withResource pool . flip userList' $ (h, mo, ml)

--POOL.withResource pool $ \conn -> userList' conn (h, mo, ml)

-- POOL.withResource pool . flip $ userList' (h, mo, ml)

{--do
maybeConn <- liftIO $ POOL.tryTakeResource conns
case maybeConn of
  Nothing -> do
    liftIO $ Logger.logError (News.hLogHandle h) "lookupAccount: BAD! error500 "
    throwError err500 {errReasonPhrase = show ErrorTypes.error500}
  Just (conn, _) -> do
    account <- liftIO $ EX.runExceptT $ userList' conn (h, mo, ml)

withPool :: DataTypes.Handle -> (DataTypes.StatePool -> IO c) -> IO c
withPool h = EXS.bracket (initConnPool h) POOL.destroyAllResources
    --}

userList' ::
  SQL.Connection ->
  (News.Handle IO, Maybe DataTypes.Offset, Maybe DataTypes.Limit) ->
  IO (Either ErrorTypes.GetContentError [DataTypes.User])
userList' conn (h, mo, ml) = EX.runExceptT $ userListExcept conn (h, mo, ml)

userListExcept ::
  SQL.Connection ->
  (News.Handle IO, Maybe DataTypes.Offset, Maybe DataTypes.Limit) ->
  EX.ExceptT ErrorTypes.GetContentError IO [DataTypes.User]
userListExcept conn (h, mo, ml) = do
  liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["Request: Get User List with offset = ", T.pack $ show mo, " limit = ", T.pack $ show ml]
  (offset, limit) <- EX.withExceptT ErrorTypes.InvalidOffsetOrLimitGetContent $ OffsetLimit.checkOffsetLimit h mo ml
  res <-
    liftIO
      ( EXS.try $
          SQL.query
            conn
            [sql|SELECT usr_name, usr_login, usr_admin, usr_author, usr_created
               FROM usr ORDER BY usr_created LIMIT ?  OFFSET ? |]
            (show limit, show offset) ::
          IO (Either SQL.SqlError [(T.Text, String, Bool, Bool, TIME.Day)])
      )
  case res of
    Left _ -> do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidCookie "userListExcept: BAD! Connection disconnected")
      EX.throwE $ ErrorTypes.GetContentSQLRequestError $ ErrorTypes.SQLRequestError []
    Right rez -> do
      let users = Prelude.map Lib.toUser rez
      let toTextUsers = T.concat $ map ToText.toText users
      liftIO $ Logger.logDebug (News.hLogHandle h) $ T.concat ["userList: OK! \n", toTextUsers]
      return users

{--
userListExcept ::
  SQL.Connection ->
  (News.Handle IO, Maybe DataTypes.Offset, Maybe DataTypes.Limit) ->
  EX.ExceptT ErrorTypes.GetContentError IO [DataTypes.User]
userListExcept conn (h, mo, ml) = do
  liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["Request: Get User List with offset = ", T.pack $ show mo, " limit = ", T.pack $ show ml]
  (offset, limit) <- EX.withExceptT ErrorTypes.InvalidOffsetOrLimitGetContent $ OffsetLimit.checkOffsetLimit h mo ml
  --- conn <- EX.withExceptT ErrorTypes.GetContentSQLRequestError $ DbConnect.tryRequestConnectDb h
  res <-
    liftIO $
      SQL.query
        conn
        [sql|SELECT usr_name, usr_login, usr_admin, usr_author, usr_created
               FROM usr ORDER BY usr_created LIMIT ?  OFFSET ? |]
        (show limit, show offset)
  let users = Prelude.map Lib.toUser res
  let toTextUsers = T.concat $ map ToText.toText users
  liftIO $ Logger.logDebug (News.hLogHandle h) $ T.concat ["userList: OK! \n", toTextUsers]
  return users
--}
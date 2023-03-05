module EndPoints.GetUserList
  ( getUserList,
    userList,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.OffsetLimit as OffsetLimit
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logDebug, logInfo)
import qualified News
import Servant (Handler)
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes
import qualified DbConnect

getUserList ::
  News.Handle IO ->
  DataTypes.Db ->
  Maybe DataTypes.Offset ->
  Maybe DataTypes.Limit ->
  Handler [DataTypes.User]
getUserList h DataTypes.Db {..} ma ml =
  (>>=) (liftIO $ dbUserList (h, ma, ml)) ToHttpResponse.toHttpResponse

userList ::
  SQL.Connection ->
  (News.Handle IO, Maybe DataTypes.Offset, Maybe DataTypes.Limit) ->
  IO (Either ErrorTypes.GetContentError [DataTypes.User])
userList conn (h, mo, ml) = do EX.runExceptT $ userListExcept conn (h, mo, ml)

userListExcept ::
  SQL.Connection ->
  (News.Handle IO, Maybe DataTypes.Offset, Maybe DataTypes.Limit) ->
  EX.ExceptT ErrorTypes.GetContentError IO [DataTypes.User]
userListExcept _ (h, mo, ml) = do
  liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["Request: Get User List with offset = ", T.pack $ show mo, " limit = ", T.pack $ show ml]
  (offset, limit) <- EX.withExceptT ErrorTypes.InvalidOffsetOrLimitGetContent $ OffsetLimit.checkOffsetLimit h mo ml
  conn <- EX.withExceptT ErrorTypes.GetContentSQLRequestError $ DbConnect.tryRequestConnectDb h
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

module EndPoints.GetUserList
  ( getUserList,
    userList,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.Pool as POOL
import qualified Data.Text as T
import qualified Data.Time as TIME
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.OffsetLimit as OffsetLimit
import qualified EndPoints.Lib.ThrowError as Throw
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logDebug, logInfo, (.<))
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
  POOL.Pool SQL.Connection ->
  (News.Handle IO, Maybe DataTypes.Offset, Maybe DataTypes.Limit) ->
  IO (Either ErrorTypes.GetContentError [DataTypes.User])
userList pool (h, mo, ml) = EX.runExceptT $ userListExcept pool (h, mo, ml)

userListExcept ::
  POOL.Pool SQL.Connection ->
  (News.Handle IO, Maybe DataTypes.Offset, Maybe DataTypes.Limit) ->
  EX.ExceptT ErrorTypes.GetContentError IO [DataTypes.User]
userListExcept pool (h, mo, ml) = do
  liftIO $ Logger.logInfo (News.hLogHandle h) $ "\n\nRequest: Get User List with offset = " .< mo <> " limit = " .< ml
  (offset', limit') <- EX.withExceptT ErrorTypes.InvalidOffsetOrLimitGetContent $ OffsetLimit.checkOffsetLimit h mo ml
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql|SELECT usr_name, usr_login, usr_admin, usr_author, usr_created
               FROM usr ORDER BY usr_created LIMIT ?  OFFSET ? |]
                (show $ DataTypes.limit limit', show $ DataTypes.offset offset')
          ) ::
          IO (Either EXS.SomeException [(T.Text, String, Bool, Bool, TIME.Day)])
      )
  case res of
    Left err -> Throw.throwSomeException h "userListExcept" err
    Right value -> do
      let users = Prelude.map Lib.toUser value
      let toTextUsers = T.concat $ map ToText.toText users
      liftIO $ Logger.logDebug (News.hLogHandle h) $ "userList: OK! \n" <> toTextUsers
      return users

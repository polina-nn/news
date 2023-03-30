module EndPoints.AddOneUser
  ( addOneUser,
    addUser,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.Int as I
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.LibIO as LibIO
import qualified EndPoints.Lib.ThrowSqlRequestError as Throw
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logDebug, logError, logInfo, (.<))
import qualified News
import Servant (Handler)
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

addOneUser ::
  News.Handle IO ->
  DataTypes.Db ->
  DataTypes.Token ->
  DataTypes.CreateUserRequest ->
  Handler DataTypes.User
addOneUser h DataTypes.Db {..} user createUserReq =
  (>>=)
    (liftIO $ dbAddUser (h, user, createUserReq))
    ToHttpResponse.toHttpResponse

addUser ::
  SQL.Connection ->
  (News.Handle IO, DataTypes.Token, DataTypes.CreateUserRequest) ->
  IO (Either ErrorTypes.AddUserError DataTypes.User)
addUser conn (h, account, req) = EX.runExceptT $ addUserExcept conn (h, account, req)

addUserExcept ::
  SQL.Connection ->
  (News.Handle IO, DataTypes.Token, DataTypes.CreateUserRequest) ->
  EX.ExceptT ErrorTypes.AddUserError IO DataTypes.User
addUserExcept conn (h, token, req) = do
  user <- EX.withExceptT ErrorTypes.AddUserSQLRequestError (LibIO.searchUser h conn token)
  liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["Request: Add User: \n", ToText.toText req, "by user: ", ToText.toText user]
  _ <- EX.withExceptT ErrorTypes.InvalidPermissionAddUser (Lib.checkUserAdmin h user)
  _ <- checkLogin conn h req
  _ <- addTokenToDB conn h req
  addUserToDB conn h req

-- | checkLogin - check the existence of the login. Duplication of login is not allowed
checkLogin ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateUserRequest ->
  EX.ExceptT ErrorTypes.AddUserError IO DataTypes.CreateUserRequest
checkLogin conn h' r@DataTypes.CreateUserRequest {..} = do
  res <-
    liftIO
      ( EXS.try
          ( SQL.query
              conn
              [sql| SELECT EXISTS (SELECT usr_login  FROM usr WHERE usr_login = ?) |]
              (SQL.Only login)
          ) ::
          IO (Either SQL.SqlError [SQL.Only Bool])
      )

  case res of
    Left err -> Throw.throwSqlRequestError h' ("checkLogin ", show err)
    Right [SQL.Only True] -> do
      liftIO $
        Logger.logError
          (News.hLogHandle h')
          ("ERROR " .< ErrorTypes.UserAlreadyExisted (ErrorTypes.InvalidContent "checkLogin: BAD! User with this login already exists "))
      EX.throwE $ ErrorTypes.UserAlreadyExisted $ ErrorTypes.InvalidContent []
    Right [SQL.Only False] -> do
      liftIO $ Logger.logDebug (News.hLogHandle h') "checkLogin: OK!"
      return r
    Right _ -> Throw.throwSqlRequestError h' ("checkLogin ", "Developer error!")

addTokenToDB ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateUserRequest ->
  EX.ExceptT ErrorTypes.AddUserError IO DataTypes.CreateUserRequest
addTokenToDB conn h r@DataTypes.CreateUserRequest {..} = do
  let token = Lib.hashed ("key" ++ login)
  res <-
    liftIO
      ( EXS.try
          ( SQL.execute
              conn
              [sql|INSERT INTO token  (token_login, token_key )
             VALUES (?, ?) |]
              (login, token)
          ) ::
          IO (Either SQL.SqlError I.Int64)
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("addTokenToDB", show err)
    Right 1 -> do
      liftIO $ Logger.logInfo (News.hLogHandle h) "addTokenToDB: OK!"
      return r
    Right _ -> Throw.throwSqlRequestError h ("addTokenToDB", "Developer error")

addUserToDB ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateUserRequest ->
  EX.ExceptT ErrorTypes.AddUserError IO DataTypes.User
addUserToDB conn h DataTypes.CreateUserRequest {..} = do
  created <- liftIO Lib.currentDay
  res <-
    liftIO
      ( EXS.try
          ( SQL.execute
              conn
              [sql|INSERT INTO usr (usr_name, usr_login , usr_password, usr_created, usr_admin, usr_author )
             VALUES (?, ?, ?, ?, ?, ?) |]
              (name, login, Lib.hashed password, show created, admin, author)
          ) ::
          IO (Either SQL.SqlError I.Int64)
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("addUserToDB", show err)
    Right 1 -> do
      let newUser =
            ( DataTypes.User
                { userName = name,
                  userLogin = login,
                  userPassword = Nothing,
                  userCreated = created,
                  userAdmin = admin,
                  userAuthor = author
                }
            )
      liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["addUserToDB: OK!", ToText.toText newUser]
      return newUser
    Right _ -> Throw.throwSqlRequestError h ("addUserToDB", "Developer error")

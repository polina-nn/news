module EndPoints.AddOneUser
  ( addOneUser,
    addUser,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.Pool as POOL
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified DbConnect
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.LibIO as LibIO
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
  DataTypes.Account ->
  DataTypes.CreateUserRequest ->
  Handler DataTypes.User
addOneUser h DataTypes.Db {..} user createUserReq =
  (>>=)
    (liftIO $ dbAddUser (h, user, createUserReq))
    ToHttpResponse.toHttpResponse

addUser ::
  DataTypes.StatePool ->
  (News.Handle IO, DataTypes.Account, DataTypes.CreateUserRequest) ->
  IO (Either ErrorTypes.AddUserError DataTypes.User)
addUser conn (h, account, req) = undefined

{--
addUser ::
  SQL.Connection ->
  (News.Handle IO, DataTypes.Account, DataTypes.CreateUserRequest) ->
  IO (Either ErrorTypes.AddUserError DataTypes.User)
addUser conn (h, account, req) = EX.runExceptT $ addUserExcept conn (h, account, req)

addUserExcept ::
  SQL.Connection ->
  (News.Handle IO, DataTypes.Account, DataTypes.CreateUserRequest) ->
  EX.ExceptT ErrorTypes.AddUserError IO DataTypes.User
addUserExcept _ (h, account, req) = do
  conn <- EX.withExceptT ErrorTypes.AddUserSQLRequestError $ DbConnect.tryRequestConnectDb h
  user <- EX.withExceptT ErrorTypes.AddUserSQLRequestError (LibIO.searchUser h conn account)
  liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["Request: Add User: \n", ToText.toText req, "by user: ", ToText.toText user]
  _ <- EX.withExceptT ErrorTypes.InvalidPermissionAddUser (Lib.checkUserAdmin h user)
  _ <- checkLogin conn h req
  _ <- addCookieToDB conn h req
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
      ( SQL.query
          conn
          [sql| SELECT EXISTS (SELECT usr_login  FROM usr WHERE usr_login = ?) |]
          (SQL.Only login)
      )
  case res of
    [x] ->
      if not (SQL.fromOnly x)
        then do
          liftIO $ Logger.logDebug (News.hLogHandle h') "checkLogin: OK!"
          return r
        else do
          liftIO $
            Logger.logError
              (News.hLogHandle h')
              ("ERROR " .< ErrorTypes.UserAlreadyExisted (ErrorTypes.InvalidContent "checkLogin: BAD! User with this login already exists "))
          EX.throwE $ ErrorTypes.UserAlreadyExisted $ ErrorTypes.InvalidContent []
    _ -> do
      liftIO $
        Logger.logError
          (News.hLogHandle h')
          ( "ERROR "
              .< ErrorTypes.AddUserSQLRequestError
                ( ErrorTypes.SQLRequestError "checkLogin: BAD! Logic error!"
                )
          )
      EX.throwE $ ErrorTypes.AddUserSQLRequestError $ ErrorTypes.SQLRequestError []

addCookieToDB ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateUserRequest ->
  EX.ExceptT ErrorTypes.AddUserError IO DataTypes.CreateUserRequest
addCookieToDB conn h r@DataTypes.CreateUserRequest {..} = do
  let cookie = "key" ++ login
  res <-
    liftIO
      ( SQL.execute
          conn
          [sql|INSERT INTO cookie  (cookie_account,  cookie_key )
             VALUES (?, ?) |]
          (login, cookie)
      )
  case read (show res) :: Int of
    1 -> do
      liftIO $ Logger.logInfo (News.hLogHandle h) $ "addCookieToDB: OK! Cookie is " .< cookie
      return r
    _ -> do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddUserSQLRequestError (ErrorTypes.SQLRequestError "addCookieToDB! Don't INSERT INTO  user table"))
      EX.throwE $ ErrorTypes.AddUserSQLRequestError $ ErrorTypes.SQLRequestError []

addUserToDB ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateUserRequest ->
  EX.ExceptT ErrorTypes.AddUserError IO DataTypes.User
addUserToDB conn h DataTypes.CreateUserRequest {..} = do
  created <- liftIO Lib.currentDay
  res <-
    liftIO
      ( SQL.execute
          conn
          [sql|INSERT INTO usr (usr_name, usr_login , usr_password, usr_created, usr_admin, usr_author )
             VALUES (?, ?, ?, ?, ?, ?) |]
          (name, login, Lib.hashed password, show created, admin, author)
      )
  case read (show res) :: Int of
    1 -> do
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
    _ -> do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddUserSQLRequestError (ErrorTypes.SQLRequestError "addUserToDB! Don't INSERT INTO  user table"))
      EX.throwE $ ErrorTypes.AddUserSQLRequestError $ ErrorTypes.SQLRequestError []

--}
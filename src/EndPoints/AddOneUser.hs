module EndPoints.AddOneUser
  ( addOneUser,
    addUser,
  )
where

import Control.Exception.Base
  ( Exception (displayException),
    SomeException (SomeException),
    catch,
    throwIO,
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Lib as Lib
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
  DataTypes.User ->
  DataTypes.CreateUserRequest ->
  Handler DataTypes.User
addOneUser h DataTypes.Db {..} user createUserReq =
  (>>=)
    (liftIO $ _addUser (h, user, createUserReq))
    ToHttpResponse.toHttpResponse

addUser ::
  SQL.Connection ->
  (News.Handle IO, DataTypes.User, DataTypes.CreateUserRequest) ->
  IO (Either ErrorTypes.AddUserError DataTypes.User)
addUser conn (h, user, req) = do
  Logger.logInfo (News.hLogHandle h) "Request: Add User "
  allCheck <- Lib.checkUserAdmin h user >>= checkLogin conn h req
  case allCheck of
    Left err -> return $ Left err
    Right _ -> do
      rez <- catch (addUserIO conn h req) handleError
      case rez of
        (Right newUser) -> do
          Logger.logInfo (News.hLogHandle h) ("addUser: OK! \n" .< ToText.toText newUser)
          return rez
        (Left newUserError) -> do
          Logger.logError
            (News.hLogHandle h)
            ("addUser: BAD! " .< newUserError)
          return rez
  where
    handleError ::
      SomeException -> IO (Either ErrorTypes.AddUserError DataTypes.User)
    handleError (SomeException e) = do
      Logger.logInfo (News.hLogHandle h) ("addUser:handleError: throwIO is unknown error" .< displayException e)
      throwIO e

addUserIO ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateUserRequest ->
  IO (Either ErrorTypes.AddUserError DataTypes.User)
addUserIO conn h DataTypes.CreateUserRequest {..} = do
  created <- Lib.currentDay
  res <-
    SQL.execute
      conn
      [sql|INSERT INTO usr (usr_name, usr_login , usr_password, usr_created, usr_admin, usr_author )
             VALUES (?, ?, ?, ?, ?, ?) ;|]
      (name, login, Lib.hashed password, show created, admin, author)
  Logger.logInfo
    (News.hLogHandle h)
    ("addUserIO: OK! INSERT INTO \n" .< res)
  case read (show res) :: Int of
    1 ->
      return
        ( Right $
            DataTypes.User
              { userName = name,
                userLogin = login,
                userPassword = Nothing, -- Do not show password
                userCreated = created,
                userAdmin = admin,
                userAuthor = author
              }
        )
    _ -> do
      Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddUserSQLRequestError (ErrorTypes.SQLRequestError "addUserIO! Don't INSERT INTO  user table"))
      return $
        Left $ ErrorTypes.AddUserSQLRequestError $ ErrorTypes.SQLRequestError []

-- | checkLogin - check the existence of the login. Duplication of login is not allowed
checkLogin ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateUserRequest ->
  Either ErrorTypes.InvalidAdminPermission DataTypes.User ->
  IO (Either ErrorTypes.AddUserError DataTypes.CreateUserRequest)
checkLogin _ _ _ (Left err) =
  return $ Left $ ErrorTypes.InvalidPermissionAddUser err
checkLogin conn h' r@DataTypes.CreateUserRequest {..} (Right _) = do
  res <-
    SQL.query
      conn
      [sql| SELECT EXISTS (SELECT usr_login  FROM usr WHERE usr_login = ?) |]
      (SQL.Only login) ::
      IO [SQL.Only Bool]
  case res of
    [x] ->
      if not (SQL.fromOnly x)
        then do
          Logger.logDebug (News.hLogHandle h') ("checkLogin: OK! Not exists user with login " .< show login)
          return $ Right r
        else do
          Logger.logError
            (News.hLogHandle h')
            ( "ERROR "
                .< ErrorTypes.UserAlreadyExisted
                  ( ErrorTypes.InvalidContent
                      ( "checkLogin: BAD! Exists user with login "
                          ++ show login
                      )
                  )
            )
          return $
            Left $ ErrorTypes.UserAlreadyExisted $ ErrorTypes.InvalidContent []
    _ -> do
      Logger.logError
        (News.hLogHandle h')
        ( "ERROR "
            .< ErrorTypes.AddUserSQLRequestError
              ( ErrorTypes.SQLRequestError "checkLogin: BAD! Logic error!"
              )
        )
      return $
        Left $ ErrorTypes.AddUserSQLRequestError $ ErrorTypes.SQLRequestError []

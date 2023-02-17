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
import qualified Control.Monad.Trans.Except as EX
import qualified Data.Text as T
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
    (liftIO $ dbAddUser (h, user, createUserReq))
    ToHttpResponse.toHttpResponse

addUser ::
  SQL.Connection ->
  (News.Handle IO, DataTypes.User, DataTypes.CreateUserRequest) ->
  IO (Either ErrorTypes.AddUserError DataTypes.User)
addUser conn (h, user, req) = do
  Logger.logInfo (News.hLogHandle h) $ T.concat ["Request: Add User: \n", ToText.toText req, "by user: ", ToText.toText user]
  allCheck' <- allCheck conn (h, user, req) >>= EX.runExceptT
  case allCheck' of
    Left err -> return $ Left err
    Right _ -> do
      rez <- catch (addUserIO conn h req) handleError
      case rez of
        (Right newUser) -> do
          Logger.logInfo (News.hLogHandle h) $ T.concat ["addUser: OK! \n", ToText.toText newUser]
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
             VALUES (?, ?, ?, ?, ?, ?) |]
      (name, login, Lib.hashed password, show created, admin, author)
  case read (show res) :: Int of
    1 ->
      return
        ( Right $
            DataTypes.User
              { userName = name,
                userLogin = login,
                userPassword = Nothing,
                userCreated = created,
                userAdmin = admin,
                userAuthor = author
              }
        )
    _ -> do
      Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddUserSQLRequestError (ErrorTypes.SQLRequestError "addUserIO! Don't INSERT INTO  user table"))
      return $
        Left $ ErrorTypes.AddUserSQLRequestError $ ErrorTypes.SQLRequestError []

-- | allCheck  -  check permission for add user; check the existence of the login. Duplication of login is not allowed
allCheck :: SQL.Connection -> (News.Handle IO, DataTypes.User, DataTypes.CreateUserRequest) -> IO (EX.ExceptT ErrorTypes.AddUserError IO DataTypes.CreateUserRequest)
allCheck conn (h, user, createUser) = do
  checkUserAdmin'' <- EX.withExceptT ErrorTypes.InvalidPermissionAddUser <$> Lib.checkUserAdmin' h user
  checkLogin' <- checkLogin conn h createUser
  let checkLogin'' = do { checkUserAdmin'' >> checkLogin' } `EX.catchE` EX.throwE
  return checkLogin''

-- | checkLogin - check the existence of the login. Duplication of login is not allowed
checkLogin ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateUserRequest ->
  IO (EX.ExceptT ErrorTypes.AddUserError IO DataTypes.CreateUserRequest)
checkLogin conn h' r@DataTypes.CreateUserRequest {..} = do
  res <-
    SQL.query
      conn
      [sql| SELECT EXISTS (SELECT usr_login  FROM usr WHERE usr_login = ?) |]
      (SQL.Only login)
  case res of
    [x] ->
      if not (SQL.fromOnly x)
        then do
          Logger.logDebug (News.hLogHandle h') "checkLogin: OK!"
          return $ pure r
        else do
          Logger.logError
            (News.hLogHandle h')
            ("ERROR " .< ErrorTypes.UserAlreadyExisted (ErrorTypes.InvalidContent "checkLogin: BAD! User with this login already exists "))
          return . EX.throwE $ ErrorTypes.UserAlreadyExisted $ ErrorTypes.InvalidContent []
    _ -> do
      Logger.logError
        (News.hLogHandle h')
        ( "ERROR "
            .< ErrorTypes.AddUserSQLRequestError
              ( ErrorTypes.SQLRequestError "checkLogin: BAD! Logic error!"
              )
        )
      return . EX.throwE $ ErrorTypes.AddUserSQLRequestError $ ErrorTypes.SQLRequestError []

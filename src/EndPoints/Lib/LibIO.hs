-- |  EndPoints.Lib.Lib - library of helper IO functions for EndPoints
module EndPoints.Lib.LibIO (searchUser) where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.Pool as POOL
import qualified Data.Text as T
import qualified Data.Time as TIME
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Lib as Lib
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

-- | searchUser - searchUser by token in database.
searchUser ::
  POOL.Pool SQL.Connection ->
  DataTypes.Token ->
  EX.ExceptT ErrorTypes.SearchUserError IO DataTypes.User
searchUser pool DataTypes.Token {..} = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql|SELECT usr_name, usr_login, usr_admin, usr_author, usr_created 
               FROM usr LEFT JOIN token ON usr.usr_login = token.token_login
               WHERE token_key = ? |]
                (SQL.Only token)
          ) ::
          IO (Either EXS.SomeException [(T.Text, String, Bool, Bool, TIME.Day)])
      )
  case res of
    Left err -> EXS.throwM $ ErrorTypes.SearchUseSomeException err
    Right [user] -> return $ Lib.toUser user
    Right [] -> EXS.throwM $ ErrorTypes.SearchUserNotExist $ ErrorTypes.SQLRequestError " Token exists, but not user "
    Right _ -> EXS.throwM $ ErrorTypes.SearchUserSQLRequestError $ ErrorTypes.SQLRequestError "Developer error!"

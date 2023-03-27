-- |  EndPoints.Lib.Lib - library of helper IO functions for EndPoints
module EndPoints.Lib.LibIO (searchUser) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Lib as Lib
import Logger (logError, (.<))
import qualified News
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

-- | searchUser - searchUser by token in database.
searchUser ::
  News.Handle IO ->
  SQL.Connection ->
  DataTypes.Token ->
  EX.ExceptT ErrorTypes.SQLRequestError IO DataTypes.User
searchUser h conn DataTypes.Token {..} = do
  res <-
    liftIO $
      SQL.query
        conn
        [sql|SELECT usr_name, usr_login, usr_admin, usr_author, usr_created 
               FROM usr LEFT JOIN token ON usr.usr_login = token.token_login
               WHERE token_key = ? |]
        (SQL.Only token)
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

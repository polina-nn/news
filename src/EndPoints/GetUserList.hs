{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module EndPoints.GetUserList
  ( getUserList
  , userList
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Text as T
import qualified Data.Time as TIME
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.OffsetLimit as OffsetLimit
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import qualified Logger
import qualified News
import Servant (Handler)
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

getUserList ::
     News.Handle IO
  -> DataTypes.Db
  -> Maybe DataTypes.Offset
  -> Maybe DataTypes.Limit
  -> Handler [DataTypes.User]
getUserList h DataTypes.Db {..} ma ml =
  (>>=) (liftIO $ _userList (h, ma, ml)) ToHttpResponse.toHttpResponse

userList ::
     SQL.Connection
  -> (News.Handle IO, Maybe DataTypes.Offset, Maybe DataTypes.Limit)
  -> IO (Either ErrorTypes.GetContentError [DataTypes.User])
userList conn (h, mo, ml) = do
  Logger.logInfo (News.hLogHandle h) $ T.pack "Request: Get User List "
  rezCheckOffsetLimit <- OffsetLimit.checkOffsetLimit h mo ml
  case rezCheckOffsetLimit of
    Left err -> return $ Left err
    Right (offset, limit) -> do
      res <-
        SQL.query
          conn
          [sql| SELECT usr_name, usr_login, usr_admin, usr_author, usr_created 
                FROM usr 
                ORDER BY usr_created 
                LIMIT ?  OFFSET ? |]
          (show limit, show offset)
      let users = Prelude.map toUser res
      let toTextUsers = (T.concat $ map ToText.toText users) :: T.Text
      Logger.logDebug (News.hLogHandle h) $
        T.concat [T.pack "userList: OK! \n", toTextUsers]
      return $ Right users

toUser :: (T.Text, String, Bool, Bool, TIME.Day) -> DataTypes.User
toUser (user_name, user_login, user_admin, user_author, user_created) =
  let user_password = Nothing
   in DataTypes.User {..}

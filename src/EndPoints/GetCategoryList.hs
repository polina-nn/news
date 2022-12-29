{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module EndPoints.GetCategoryList
  ( getCategoryList,
    categoryList,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified DbException
import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.OffsetLimit as OffsetLimit
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import qualified Logger
import qualified News
import Servant (Handler)
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

getCategoryList ::
  News.Handle IO ->
  DataTypes.Db ->
  Maybe DataTypes.Offset ->
  Maybe DataTypes.Limit ->
  Handler [DataTypes.Category]
getCategoryList h DataTypes.Db {..} ma ml =
  (>>=) (liftIO $ _categoryList (h, ma, ml)) ToHttpResponse.toHttpResponse

categoryList ::
  SQL.Connection ->
  (News.Handle IO, Maybe DataTypes.Offset, Maybe DataTypes.Limit) ->
  IO (Either ErrorTypes.GetContentError [DataTypes.Category])
categoryList _ (h, mo, ml) = do
  Logger.logInfo (News.hLogHandle h) $ T.pack "Request: Get Category List "
  rezCheckOffsetLimit <- OffsetLimit.checkOffsetLimit h mo ml
  case rezCheckOffsetLimit of
    Left err -> return $ Left err
    Right (o, l) -> do
      tryConnectDb <- DbException.tryRequestConnectDb h
      case tryConnectDb of
        Left err -> return $ Left $ ErrorTypes.GetContentSQLRequestError err
        Right conn -> do
          res <-
            SQL.query
              conn
              [sql| SELECT category_path, category_id, category_name
                     FROM category 
                     ORDER BY category_path 
                     LIMIT ?  OFFSET ? |]
              (show l, show o)
          let categories = Prelude.map Category.toCategories res
          let toTextCategories = (T.concat $ map ToText.toText categories) :: T.Text
          Logger.logDebug (News.hLogHandle h) $
            T.concat [T.pack "categoryList: OK! \n", toTextCategories]
          liftIO $ SQL.close conn
          return $ Right categories

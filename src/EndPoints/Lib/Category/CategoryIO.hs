module EndPoints.Lib.Category.CategoryIO
  ( changePathCategories,
    getAllCategories,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.Int as I
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.Category.CategoryHelpTypes as CategoryHelpTypes
import qualified EndPoints.Lib.ThrowSqlRequestError as Throw
import Logger (logDebug)
import qualified News
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

changePathCategories ::
  SQL.Connection ->
  News.Handle IO ->
  [CategoryHelpTypes.EditCategory] ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO Int
changePathCategories _ h [] = do
  liftIO $
    Logger.logDebug
      (News.hLogHandle h)
      "changePathCategories: OK! Request don't change path"
  return 0
changePathCategories conn h rs = count
  where
    count :: EX.ExceptT ErrorTypes.AddEditCategoryError IO Int
    count = do
      rez <- mapM (changePathOneCategory conn h) rs
      liftIO $ Logger.logDebug (News.hLogHandle h) $ T.concat ["changePathCategories: OK! Request change path of ", T.pack $ show $ sum rez, " categories "]
      return $ sum rez

changePathOneCategory ::
  SQL.Connection ->
  News.Handle IO ->
  CategoryHelpTypes.EditCategory ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO Int
changePathOneCategory conn h CategoryHelpTypes.EditCategory {..} = do
  res <-
    liftIO
      ( EXS.try $
          SQL.execute
            conn
            [sql| UPDATE  category SET category_path = ? WHERE category_id = ? |]
            (newPath, permanentId) ::
          IO (Either SQL.SqlError I.Int64)
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("changePathOneCategory", show err)
    Right 1 -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) $ T.concat ["changePathOneCategory: OK! \n  id = ", T.pack $ show permanentId, " path = ", T.pack $ show newPath]
      return 1
    Right _ -> Throw.throwSqlRequestError h ("changePathOneCategory", "Developer error")

-- | getAllCategories --
-- when we add the first category to the response, we get an empty array in getAllCategories
getAllCategories ::
  SQL.Connection ->
  News.Handle IO ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO [DataTypes.Category]
getAllCategories conn h = do
  res <-
    liftIO
      ( EXS.try
          ( SQL.query_
              conn
              [sql| SELECT category_path, category_id, category_name FROM category ORDER BY category_path |]
          ) ::
          IO (Either SQL.SqlError [(DataTypes.Path, DataTypes.Id, DataTypes.Name)])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("getAllCategories", show err)
    Right [] -> return []
    Right val -> return $ Prelude.map Category.toCategories val

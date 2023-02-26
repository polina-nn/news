module EndPoints.Lib.Category.CategoryIO
  ( changePathCategories,
    getAllCategories,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.Category.CategoryHelpTypes as CategoryHelpTypes
import Logger (logDebug, logError, (.<))
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
      ( SQL.execute
          conn
          [sql| UPDATE  category SET category_path = ? WHERE category_id = ? |]
          (newPath, permanentId)
      )
  case read $ show res :: Int of
    1 -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) $ T.concat ["changePathOneCategory: OK! \n  id = ", T.pack $ show permanentId, " path = ", T.pack $ show newPath]
      return 1
    _ -> do
      liftIO $
        Logger.logError
          (News.hLogHandle h)
          ( "ERROR "
              .< ErrorTypes.AddEditCategorySQLRequestError
                ( ErrorTypes.SQLRequestError
                    "changePathOneCategory:BAD! Don't UPDATE  category"
                )
          )
      EX.throwE $
        ErrorTypes.AddEditCategorySQLRequestError $
          ErrorTypes.SQLRequestError []

-- | getAllCategories --
-- when we add the first category to the response, we get an empty array in getAllCategories
getAllCategories ::
  SQL.Connection ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO [DataTypes.Category]
getAllCategories conn = do
  res <-
    liftIO
      ( SQL.query_
          conn
          [sql| SELECT category_path, category_id, category_name FROM category ORDER BY category_path |]
      )
  case res of
    [] -> return []
    _ -> return $ Prelude.map Category.toCategories res

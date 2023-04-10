module EndPoints.EditOneCategory
  ( editOneCategory,
    editCategory,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.Int as I
import qualified Data.Pool as POOL
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Category.CategoryIO as CategoryIO
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

editOneCategory ::
  News.Handle IO ->
  DataTypes.Db ->
  DataTypes.Token ->
  Int ->
  DataTypes.EditCategoryRequest ->
  Handler DataTypes.Category
editOneCategory h DataTypes.Db {..} user catId r =
  (>>=)
    (liftIO $ dbEditCategory (h, user, catId, r))
    ToHttpResponse.toHttpResponse

editCategory ::
  POOL.Pool SQL.Connection ->
  (News.Handle IO, DataTypes.Token, Int, DataTypes.EditCategoryRequest) ->
  IO (Either ErrorTypes.AddEditCategoryError DataTypes.Category)
editCategory pool (h, token, catId, r) = EX.runExceptT $ editCategoryExcept pool (h, token, catId, r)

editCategoryExcept ::
  POOL.Pool SQL.Connection ->
  (News.Handle IO, DataTypes.Token, Int, DataTypes.EditCategoryRequest) ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.Category
editCategoryExcept pool (h, token, catId, r) = do
  _ <- checkId pool h catId
  user <- EX.withExceptT ErrorTypes.AddEditCategorySQLRequestError (LibIO.searchUser h pool token)
  liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["\n\nRequest: Edit Category: \n", ToText.toText r, "with category id ", T.pack $ show catId, "\nby user: ", ToText.toText user]
  _ <- EX.withExceptT ErrorTypes.InvalidPermissionAddEditCategory (Lib.checkUserAdmin h user)
  _ <- checkCategoryName pool h r
  _ <- checkParentId pool h r
  _ <- checkParentNotHisChild pool h catId r
  _ <- editCategoryParent pool h catId r
  _ <- editCategoryName pool h catId r
  CategoryIO.getCategoryById pool h catId

-- | checkIdIO  - check if there is a record with the given category id in the database ( id = 7 in http://localhost:8080/category/7 )
checkId ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  Int ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO Int
checkId pool h' id' = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| SELECT EXISTS (SELECT category_id  FROM category WHERE category_id = ?) |]
                (SQL.Only id')
          ) ::
          IO (Either EXS.SomeException [SQL.Only Bool])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h' ("checkId", show err)
    Right [SQL.Only True] -> do
      liftIO $ Logger.logDebug (News.hLogHandle h') "checkId: OK!  Category exist "
      return id'
    Right [SQL.Only False] -> do
      liftIO $ Logger.logError (News.hLogHandle h') ("ERROR " .< ErrorTypes.InvalidImagedId (ErrorTypes.InvalidId ("checkId: BAD! Not exists category with id " <> show id')))
      EX.throwE $ ErrorTypes.InvalidCategoryId $ ErrorTypes.InvalidId []
    Right _ -> Throw.throwSqlRequestError h' ("checkId", "Developer error")

-- | checkCategoryName - check the existence of the category with the same name. Duplication of category name is not allowed
checkCategoryName ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.EditCategoryRequest ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.EditCategoryRequest
checkCategoryName _ _ r@DataTypes.EditCategoryRequest {newCategory = Nothing} = return r
checkCategoryName pool h' r@DataTypes.EditCategoryRequest {newCategory = Just name} = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| SELECT EXISTS (SELECT category_name  FROM category WHERE category_name = ?) |]
                (SQL.Only name)
          ) ::
          IO (Either EXS.SomeException [SQL.Only Bool])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h' ("checkCategoryName", show err)
    Right [SQL.Only True] -> do
      liftIO $
        Logger.logError
          (News.hLogHandle h')
          ("ERROR " .< ErrorTypes.CategoryAlreadyExisted (ErrorTypes.InvalidContent "checkCategoryName: BAD! Category with this name already exists "))
      EX.throwE $ ErrorTypes.CategoryAlreadyExisted $ ErrorTypes.InvalidContent []
    Right [SQL.Only False] -> do
      liftIO $ Logger.logDebug (News.hLogHandle h') "checkCategoryName: OK!"
      return r
    Right _ -> Throw.throwSqlRequestError h' ("checkCategoryName", "Developer error!")

-- | checkParentId  - check the existence of the parent category
checkParentId ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.EditCategoryRequest ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.EditCategoryRequest
checkParentId _ _ r@DataTypes.EditCategoryRequest {newParent = Nothing} = return r
checkParentId _ _ r@DataTypes.EditCategoryRequest {newParent = Just 0} = return r
checkParentId pool h' r@DataTypes.EditCategoryRequest {newParent = Just parent} = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| SELECT EXISTS (SELECT category_id  FROM category WHERE category_id = ?) |]
                (SQL.Only parent)
          ) ::
          IO (Either EXS.SomeException [SQL.Only Bool])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h' ("checkParentId", show err)
    Right [SQL.Only False] -> do
      liftIO $
        Logger.logError
          (News.hLogHandle h')
          ("ERROR " .< ErrorTypes.CategoryParentNotExisted (ErrorTypes.InvalidContent "checkParentId: BAD! Category with this id not exists"))
      EX.throwE $ ErrorTypes.CategoryParentNotExisted $ ErrorTypes.InvalidContent []
    Right [SQL.Only True] -> do
      liftIO $ Logger.logDebug (News.hLogHandle h') "checkParentId: OK!"
      return r
    Right _ -> Throw.throwSqlRequestError h' ("checkParentId", "Developer error!")

checkParentNotHisChild ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  Int ->
  DataTypes.EditCategoryRequest ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.EditCategoryRequest
checkParentNotHisChild _ _ _ r@DataTypes.EditCategoryRequest {newParent = Nothing} = return r
checkParentNotHisChild _ _ _ r@DataTypes.EditCategoryRequest {newParent = Just 0} = return r
checkParentNotHisChild pool h id' r@DataTypes.EditCategoryRequest {newParent = Just parent}
  | parent == id' = do
    liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidParentId (ErrorTypes.InvalidContent "checkParentNotHisChild:  BAD!"))
    EX.throwE $ ErrorTypes.InvalidParentId $ ErrorTypes.InvalidContent []
  | otherwise = do
    currentCategory <- CategoryIO.getCategoryById pool h id'
    categoriesFutureParent <- CategoryIO.getCategoriesById pool h parent
    if any (\x -> DataTypes.categoryName x == DataTypes.categoryName currentCategory) categoriesFutureParent
      then do
        liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidParentId (ErrorTypes.InvalidContent "checkParentNotHisChild:  BAD! "))
        EX.throwE $ ErrorTypes.InvalidParentId $ ErrorTypes.InvalidContent []
      else do
        liftIO $ Logger.logDebug (News.hLogHandle h) "checkParentNotHisChild: OK!"
        return r

editCategoryName ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  Int ->
  DataTypes.EditCategoryRequest ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.EditCategoryRequest
editCategoryName _ _ _ r@DataTypes.EditCategoryRequest {newCategory = Nothing} = return r
editCategoryName pool h id' r@DataTypes.EditCategoryRequest {newCategory = Just newName} = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.execute
                conn
                [sql| UPDATE category SET category_name = ? WHERE category_id = ? |]
                (newName, id')
          ) ::
          IO (Either EXS.SomeException I.Int64)
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("editCategoryName", show err)
    Right 1 -> return r
    Right _ -> Throw.throwSqlRequestError h ("editCategoryName", "Developer error")

editCategoryParent ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  Int ->
  DataTypes.EditCategoryRequest ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.EditCategoryRequest
editCategoryParent _ _ _ r@DataTypes.EditCategoryRequest {newParent = Nothing} = return r
editCategoryParent pool h id' r@DataTypes.EditCategoryRequest {newParent = Just newParent} = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.execute
                conn
                [sql| UPDATE category SET category_parent_id = ? WHERE category_id = ? |]
                (newParent, id')
          ) ::
          IO (Either EXS.SomeException I.Int64)
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("editCategoryParent", show err)
    Right 1 -> return r
    Right _ -> Throw.throwSqlRequestError h ("editCategoryParent", "Developer error")

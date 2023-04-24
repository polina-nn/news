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
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logDebug, logInfo, (.<))
import qualified News
import Servant (Handler)
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

editOneCategory ::
  News.Handle IO ->
  DataTypes.Db ->
  DataTypes.Token ->
  DataTypes.Id DataTypes.Category ->
  DataTypes.EditCategoryRequest ->
  Handler DataTypes.Category
editOneCategory h DataTypes.Db {..} user catId r =
  (>>=)
    (liftIO $ dbEditCategory (h, user, catId, r))
    ToHttpResponse.toHttpResponse

editCategory ::
  POOL.Pool SQL.Connection ->
  (News.Handle IO, DataTypes.Token, DataTypes.Id DataTypes.Category, DataTypes.EditCategoryRequest) ->
  IO (Either ErrorTypes.AddEditCategoryError DataTypes.Category)
editCategory pool (h, token, catId, r) = do
  let reqResult = EXS.catch (editCategoryExcept pool (h, token, catId, r)) (ErrorTypes.handleAddEditCategoryError h)
  EX.runExceptT reqResult

editCategoryExcept ::
  POOL.Pool SQL.Connection ->
  (News.Handle IO, DataTypes.Token, DataTypes.Id DataTypes.Category, DataTypes.EditCategoryRequest) ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.Category
editCategoryExcept pool (h, token, catId, r) = do
  _ <- checkId pool h catId
  user <- EX.withExceptT ErrorTypes.AddEditCategorySearchUserError (EXS.catch (LibIO.searchUser pool token) (ErrorTypes.handleSearchUserError h))
  liftIO $ Logger.logInfo (News.hLogHandle h) $ "\n\nRequest: Edit Category: \n" <> ToText.toText r <> "with category id " .< catId <> "\nby user: " <> ToText.toText user
  _ <- EX.withExceptT ErrorTypes.InvalidPermissionAddEditCategory (EX.catchE (Lib.checkUserAdmin h user) (ErrorTypes.handleInvalidAdminPermission h))
  _ <- checkParentId pool h r
  _ <- checkParentNotHisChild pool h catId r
  _ <- editCategoryParent pool catId r
  _ <- editCategoryName pool catId r
  EX.withExceptT ErrorTypes.InvalidParentIdAddEditCategory (EXS.catch (CategoryIO.getCategoryById pool h catId) (ErrorTypes.handleInvalidContentCategoryId h))

-- | checkIdIO  - check if there is a record with the given category id in the database ( id = 7 in http://localhost:8080/category/7 )
checkId ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.Id DataTypes.Category ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO (DataTypes.Id DataTypes.Category)
checkId pool h' id' =
  do
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
      Left err -> EXS.throwM $ ErrorTypes.AddEditCategorySomeException err
      Right [SQL.Only True] -> do
        liftIO $ Logger.logDebug (News.hLogHandle h') " checkId: OK!  Category exist "
        return id'
      Right [SQL.Only False] -> do
        EXS.throwM $ ErrorTypes.InvalidCategoryId $ ErrorTypes.InvalidId $ " Not exists category with id " <> show id'
      Right _ -> EXS.throwM $ ErrorTypes.AddEditCategorySQLRequestError $ ErrorTypes.SQLRequestError " Developer error"

-- | checkParentId  - check the existence of the parent category
checkParentId ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.EditCategoryRequest ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.EditCategoryRequest
checkParentId _ _ r@DataTypes.EditCategoryRequest {newParent = Nothing} = return r
checkParentId _ _ r@DataTypes.EditCategoryRequest {newParent = Just (DataTypes.Id {getId = 0})} = return r
checkParentId pool h' r@DataTypes.EditCategoryRequest {newParent = Just parent} = do
  res <- liftIO (EX.runExceptT (EX.withExceptT ErrorTypes.InvalidParentIdAddEditCategory (EXS.catch (CategoryIO.checkCategoryExistsById pool h' parent) (ErrorTypes.handleInvalidContentCategoryId h'))))
  case res of
    Left err -> EXS.throwM err
    Right _ -> return r

checkParentNotHisChild ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.Id DataTypes.Category ->
  DataTypes.EditCategoryRequest ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.EditCategoryRequest
checkParentNotHisChild _ _ _ r@DataTypes.EditCategoryRequest {newParent = Nothing} = return r
checkParentNotHisChild _ _ _ r@DataTypes.EditCategoryRequest {newParent = Just (DataTypes.Id {getId = 0})} = return r
checkParentNotHisChild pool h id' r@DataTypes.EditCategoryRequest {newParent = Just parent}
  | parent == id' = EXS.throwM $ ErrorTypes.InvalidParentIdAddEditCategory $ ErrorTypes.InvalidContentCategoryIdError $ ErrorTypes.InvalidContent " New parent  is his child"
  | otherwise = do
    currentCategory <- EX.withExceptT ErrorTypes.InvalidParentIdAddEditCategory (EXS.catch (CategoryIO.getCategoryById pool h id') (ErrorTypes.handleInvalidContentCategoryId h))
    categoriesFutureParent <- EX.withExceptT ErrorTypes.InvalidParentIdAddEditCategory (EXS.catch (CategoryIO.getCategoriesById pool h parent) (ErrorTypes.handleInvalidContentCategoryId h))
    if any (\x -> DataTypes.categoryName x == DataTypes.categoryName currentCategory) categoriesFutureParent
      then EXS.throwM $ ErrorTypes.InvalidParentIdAddEditCategory $ ErrorTypes.InvalidContentCategoryIdError $ ErrorTypes.InvalidContent " New parent  is his child"
      else do
        liftIO $ Logger.logDebug (News.hLogHandle h) " checkParentNotHisChild: OK!"
        return r

editCategoryName ::
  POOL.Pool SQL.Connection ->
  DataTypes.Id DataTypes.Category ->
  DataTypes.EditCategoryRequest ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.EditCategoryRequest
editCategoryName _ _ r@DataTypes.EditCategoryRequest {newCategory = Nothing} = return r
editCategoryName pool id' r@DataTypes.EditCategoryRequest {newCategory = Just newName} = do
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
    Left err -> handleError err
    Right 1 -> return r
    Right _ -> EXS.throwM $ ErrorTypes.AddEditCategorySQLRequestError $ ErrorTypes.SQLRequestError " Developer error"
  where
    handleError (EXS.SomeException e) =
      let errMsg = EXS.displayException e
       in if "duplicate key value" `T.isInfixOf` T.pack errMsg
            then EXS.throwM $ ErrorTypes.CategoryAlreadyExisted $ ErrorTypes.InvalidContent " Category with this name already exists"
            else EXS.throwM $ ErrorTypes.AddEditCategorySomeException (EXS.SomeException e)

editCategoryParent ::
  POOL.Pool SQL.Connection ->
  DataTypes.Id DataTypes.Category ->
  DataTypes.EditCategoryRequest ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.EditCategoryRequest
editCategoryParent _ _ r@DataTypes.EditCategoryRequest {newParent = Nothing} = return r
editCategoryParent pool id' r@DataTypes.EditCategoryRequest {newParent = Just newParent} = do
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
    Left err -> EXS.throwM $ ErrorTypes.AddEditCategorySomeException err
    Right 1 -> return r
    Right _ -> EXS.throwM $ ErrorTypes.AddEditCategorySQLRequestError $ ErrorTypes.SQLRequestError " Developer error"

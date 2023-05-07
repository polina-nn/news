module EndPoints.AddOneCategory
  ( addOneCategory,
    addCategory,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.Pool as POOL
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Category.CategoryIO as CategoryIO
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.LibIO as LibIO
import qualified EndPoints.Lib.ThrowError as Throw
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logInfo)
import qualified News
import Servant (Handler)
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

addOneCategory ::
  News.Handle IO ->
  DataTypes.Db ->
  DataTypes.Token ->
  DataTypes.CreateCategoryRequest ->
  Handler DataTypes.Category
addOneCategory h DataTypes.Db {..} user createCategoryReq =
  (>>=)
    (liftIO $ dbAddCategory (h, user, createCategoryReq))
    ToHttpResponse.toHttpResponse

addCategory ::
  POOL.Pool SQL.Connection ->
  (News.Handle IO, DataTypes.Token, DataTypes.CreateCategoryRequest) ->
  IO (Either ErrorTypes.AddEditCategoryError DataTypes.Category)
addCategory pool (h, token, r) = EX.runExceptT $ addCategoryExcept pool (h, token, r)

addCategoryExcept ::
  POOL.Pool SQL.Connection ->
  (News.Handle IO, DataTypes.Token, DataTypes.CreateCategoryRequest) ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.Category
addCategoryExcept pool (h, token, r@DataTypes.CreateCategoryRequest {..}) =
  do
    user <- EX.withExceptT ErrorTypes.AddEditCategorySearchUserError (LibIO.searchUser pool h token)
    liftIO $ Logger.logInfo (News.hLogHandle h) $ "\n\nRequest: Add Category: \n" <> ToText.toText r <> "by user: " <> ToText.toText user
    _ <- EX.withExceptT ErrorTypes.InvalidPermissionAddEditCategory (Lib.checkUserAdmin h user)
    _ <- checkParentId pool h parent
    addCategoryToDb pool h r

-- | checkParentId  - check the existence of the parent category
checkParentId ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.Id DataTypes.Category ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO (DataTypes.Id DataTypes.Category)
checkParentId _ _ catId@DataTypes.Id {getId = 0} = return catId
checkParentId pool h parent = EX.withExceptT ErrorTypes.InvalidParentIdAddEditCategory (CategoryIO.checkCategoryExistsById pool h parent)

addCategoryToDb ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateCategoryRequest ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.Category
addCategoryToDb pool h DataTypes.CreateCategoryRequest {..} = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| INSERT INTO  category (category_parent_id, category_name)  VALUES (?,?) RETURNING category_id  |]
                (parent, category)
          ) ::
          IO (Either EXS.SomeException [SQL.Only (DataTypes.Id DataTypes.Category)])
      )
  case res of
    Left err -> handleError err
    Right [SQL.Only val] -> do
      let newCategory =
            ( DataTypes.Category
                { categoryParentId = parent,
                  categoryId = val,
                  categoryName = category
                }
            )
      liftIO $ Logger.logInfo (News.hLogHandle h) $ "addCategoryToDb: added new category: " <> ToText.toText newCategory
      return newCategory
    Right _ -> Throw.throwSqlRequestError h "addCategoryToDb " (ErrorTypes.SQLRequestError "Developer error!")
  where
    handleError (EXS.SomeException e) =
      let errMsg = EXS.displayException e
       in if "duplicate key value" `T.isInfixOf` T.pack errMsg
            then Throw.throwAlreadyExists h "addCategoryToDb " $ ErrorTypes.InvalidContent " Category with this name already exists"
            else Throw.throwSomeException h "addCategoryToDb  " (EXS.SomeException e)

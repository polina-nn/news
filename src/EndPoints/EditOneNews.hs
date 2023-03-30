module EndPoints.EditOneNews
  ( editOneNews,
    editNews,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Int as I
import qualified Data.Text as T
import qualified Data.Time as TIME
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Database.PostgreSQL.Simple.Types as SQLTypes
import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.LibIO as LibIO
import qualified EndPoints.Lib.News.NewsIO as NewsIO
import qualified EndPoints.Lib.ThrowSqlRequestError as Throw
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logDebug, logError, logInfo, (.<))
import qualified News
import Servant (Handler)
import qualified System.Directory as SD
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

type IdNews = Int

type IdCategory = Int

type IdImage = Int

type CategoryPath = String

editOneNews ::
  News.Handle IO ->
  DataTypes.Db ->
  DataTypes.Token ->
  IdNews ->
  DataTypes.EditNewsRequest ->
  Handler DataTypes.News
editOneNews h DataTypes.Db {..} user catId r =
  (>>=) (liftIO $ dbEditNews (h, user, catId, r)) ToHttpResponse.toHttpResponse

editNews ::
  SQL.Connection ->
  (News.Handle IO, DataTypes.Token, IdNews, DataTypes.EditNewsRequest) ->
  IO (Either ErrorTypes.AddEditNewsError DataTypes.News)
editNews conn (h, token, newsId, r) = do EX.runExceptT $ editNewsExcept conn (h, token, newsId, r)

editNewsExcept ::
  SQL.Connection ->
  (News.Handle IO, DataTypes.Token, IdNews, DataTypes.EditNewsRequest) ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.News
editNewsExcept conn (h, token, newsId, r) = do
  _ <- checkId conn h newsId
  user <- EX.withExceptT ErrorTypes.AddEditNewsSQLRequestError (LibIO.searchUser h conn token)
  liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["Request: Edit One News \n", ToText.toText r, "with news id ", T.pack $ show newsId, "\nby user: ", ToText.toText user]
  _ <- checkUserThisNewsAuthor conn h user newsId
  _ <- checkImageFilesExist h r
  _ <- checkPngImages h r
  _ <- checkBase64Images h r
  _ <- checkCategoryId conn h r
  _ <- newTitle conn h newsId r
  _ <- newCatId conn h newsId r
  _ <- newText conn h newsId r
  _ <- newImages conn h r
  _ <- newPublish conn h newsId r
  newsCategory <- getNewsCategory conn h newsId
  idImages <- getNewsImages conn h newsId
  getNews conn h user newsId newsCategory idImages

newTitle ::
  SQL.Connection ->
  News.Handle IO ->
  IdNews ->
  DataTypes.EditNewsRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.EditNewsRequest
newTitle _ _ _ r@DataTypes.EditNewsRequest {newTitle = Nothing} =
  return r
newTitle conn h newsId r@DataTypes.EditNewsRequest {newTitle = Just title} = do
  res <-
    liftIO
      ( EXS.try $
          SQL.execute
            conn
            [sql| UPDATE news SET news_title = ? WHERE news_id = ? |]
            (title, newsId) ::
          IO (Either SQL.SqlError I.Int64)
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("newTile", show err)
    Right 1 -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) "newTile: OK! UPDATE news "
      return r
    Right _ -> Throw.throwSqlRequestError h ("newTile", "Developer error")

newCatId ::
  SQL.Connection ->
  News.Handle IO ->
  IdNews ->
  DataTypes.EditNewsRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.EditNewsRequest
newCatId _ _ _ r@DataTypes.EditNewsRequest {newCategoryId = Nothing} =
  return r
newCatId conn h newsId r@DataTypes.EditNewsRequest {newCategoryId = Just catId} = do
  res <-
    liftIO
      ( EXS.try $
          SQL.execute
            conn
            [sql| UPDATE news SET news_category_id = ? WHERE news_id = ? |]
            (catId, newsId) ::
          IO (Either SQL.SqlError I.Int64)
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("newCatId", show err)
    Right 1 -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) "newCatId: OK! UPDATE news "
      return r
    Right _ -> Throw.throwSqlRequestError h ("newCatId", "Developer error")

newText ::
  SQL.Connection ->
  News.Handle IO ->
  IdNews ->
  DataTypes.EditNewsRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.EditNewsRequest
newText _ _ _ r@DataTypes.EditNewsRequest {newText = Nothing} =
  return r
newText conn h newsId r@DataTypes.EditNewsRequest {newText = Just text} = do
  res <-
    liftIO
      ( EXS.try $
          SQL.execute
            conn
            [sql| UPDATE news SET news_text = ? WHERE news_id = ? |]
            (text, newsId) ::
          IO (Either SQL.SqlError I.Int64)
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("newText", show err)
    Right 1 -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) "newText: OK! UPDATE news "
      return r
    Right _ -> Throw.throwSqlRequestError h ("newText", "Developer error")

newImages ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.EditNewsRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.EditNewsRequest
newImages _ _ r@DataTypes.EditNewsRequest {newImages = Nothing} =
  return r
newImages conn h r@DataTypes.EditNewsRequest {newImages = Just req} = do
  mapM_ (NewsIO.addImageNews conn h) req
  return r

newPublish ::
  SQL.Connection ->
  News.Handle IO ->
  IdNews ->
  DataTypes.EditNewsRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.EditNewsRequest
newPublish _ _ _ r@DataTypes.EditNewsRequest {newPublished = Nothing} =
  return r
newPublish conn h newsId r@DataTypes.EditNewsRequest {newPublished = Just pub} = do
  res <-
    liftIO
      ( EXS.try $
          SQL.execute
            conn
            [sql| UPDATE news SET news_published = ? WHERE news_id = ? |]
            (pub, newsId) ::
          IO (Either SQL.SqlError I.Int64)
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("newPublish", show err)
    Right 1 -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) "newPublish: OK! UPDATE news "
      return r
    Right _ -> Throw.throwSqlRequestError h ("newPublish", "Developer error")

getNewsCategory ::
  SQL.Connection ->
  News.Handle IO ->
  IdNews ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO [DataTypes.Category]
getNewsCategory conn h idNews = do
  idCategory <- getNewsCategoryId conn h idNews
  categoryPath <- getCategoryPath conn h idCategory
  getCategories conn h categoryPath

getNewsCategoryId ::
  SQL.Connection ->
  News.Handle IO ->
  IdNews ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO IdCategory
getNewsCategoryId conn h idNews = do
  res <-
    liftIO
      ( EXS.try $
          SQL.query
            conn
            [sql| SELECT news_category_id   FROM news WHERE  news_id = ? |]
            (SQL.Only idNews) ::
          IO (Either SQL.SqlError [SQL.Only Int])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("getNewsCategoryId", show err)
    Right [SQL.Only categoryId] -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) ("getNewsCategoryId: OK! " .< categoryId)
      return categoryId
    Right _ -> Throw.throwSqlRequestError h ("getNewsCategoryId", "Developer error")

getCategoryPath ::
  SQL.Connection ->
  News.Handle IO ->
  IdCategory ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO CategoryPath
getCategoryPath conn h categoryId = do
  res <-
    liftIO
      ( EXS.try $
          SQL.query
            conn
            [sql| SELECT category_path  FROM category WHERE category_id = ?|]
            (SQL.Only categoryId) ::
          IO (Either SQL.SqlError [SQL.Only String])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("getCategoryPath", show err)
    Right [SQL.Only categoryPath] -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) ("getCategoryPath: OK! " .< categoryPath)
      return categoryPath
    Right _ -> Throw.throwSqlRequestError h ("getCategoryPath", "Developer error")

getCategories ::
  SQL.Connection ->
  News.Handle IO ->
  CategoryPath ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO [DataTypes.Category]
getCategories con h path = do
  res <-
    liftIO
      ( EXS.try $
          SQL.query
            con
            [sql| SELECT category_path, category_id, category_name FROM category WHERE ? LIKE category_path||'%' ORDER BY category_path|]
            (SQL.Only path) ::
          IO (Either SQL.SqlError [(DataTypes.Path, DataTypes.Id, DataTypes.Name)])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("getCategories", show err)
    Right value -> do
      let categories = Prelude.map Category.toCategories value
      liftIO $ Logger.logDebug (News.hLogHandle h) "getCategories: OK! "
      return categories

getNewsImages ::
  SQL.Connection ->
  News.Handle IO ->
  IdNews ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO [IdImage]
getNewsImages conn h idNews = do
  res <-
    liftIO
      ( EXS.try
          ( SQL.query
              conn
              [sql| SELECT EXISTS (SELECT news_images_id FROM news WHERE  news_id = ? AND news_images_id IS NOT NULL) |]
              (SQL.Only idNews)
          ) ::
          IO (Either SQL.SqlError [SQL.Only Bool])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("getNewsImages", show err)
    Right [SQL.Only True] -> getExistedNewsImages conn h idNews
    Right [SQL.Only False] -> return []
    Right _ -> Throw.throwSqlRequestError h ("getNewsImages", "Developer error!")

getExistedNewsImages ::
  SQL.Connection ->
  News.Handle IO ->
  IdNews ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO [IdImage]
getExistedNewsImages conn h idNews = do
  res <-
    liftIO
      ( EXS.try $
          SQL.query
            conn
            [sql| SELECT news_images_id  FROM news WHERE  news_id = ? |]
            (SQL.Only idNews) ::
          IO (Either SQL.SqlError [SQLTypes.Only (SQLTypes.PGArray IdImage)])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("userListExcept", show err)
    Right [ids] -> do
      let idImages = SQLTypes.fromPGArray $ SQL.fromOnly ids
      return idImages
    Right _ -> Throw.throwSqlRequestError h ("getNewsImages", "Developer error!")

getNews ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.User ->
  IdNews ->
  [DataTypes.Category] ->
  [IdImage] ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.News
getNews conn h user idNews categories imagesIds = do
  res <-
    liftIO
      ( EXS.try
          ( SQL.query
              conn
              [sql| SELECT news_title , news_created, news_text,  news_published 
                FROM news 
                WHERE  news_id = ? |]
              (SQL.Only idNews)
          ) ::
          IO (Either SQL.SqlError [(T.Text, TIME.Day, T.Text, Bool)])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("getNews", show err)
    Right [value] -> do
      let news = toNews value user categories imagesIds idNews
      liftIO $ Logger.logDebug (News.hLogHandle h) $ T.concat ["getNews: OK!", ToText.toText news]
      return news
    Right _ -> Throw.throwSqlRequestError h ("getNews", "Developer error")
  where
    toNews ::
      (T.Text, TIME.Day, T.Text, Bool) ->
      DataTypes.User ->
      [DataTypes.Category] ->
      [IdImage] ->
      IdNews ->
      DataTypes.News
    toNews (news_title, news_created, news_text, news_published) DataTypes.User {..} cats imagesId idNews' =
      DataTypes.News
        { newsTitle = news_title,
          newsCreated = news_created,
          newsAuthor = userName,
          newsCategory = cats,
          newsText = news_text,
          newsImages = Lib.imagesURIs h imagesId,
          newsPublished = news_published,
          newsId = idNews'
        }

----- All checks ----

-- | checkId - check news Id  (  id = 7 in  http://localhost:8080/news/7 )
checkId ::
  SQL.Connection ->
  News.Handle IO ->
  IdNews ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO IdNews
checkId conn h newsId = do
  res <-
    liftIO
      ( EXS.try $
          SQL.query
            conn
            [sql| SELECT EXISTS (SELECT news_id  FROM news WHERE news_id = ?) |]
            (SQL.Only newsId) ::
          IO (Either SQL.SqlError [SQL.Only Bool])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("checkId", show err)
    Right [SQL.Only True] -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) "checkId: OK! News exist "
      return newsId
    Right [SQL.Only False] -> do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidNewsId (ErrorTypes.InvalidId ("checkId: BAD! Not exists news with id " <> show newsId)))
      EX.throwE $ ErrorTypes.InvalidNewsId $ ErrorTypes.InvalidId []
    Right _ -> Throw.throwSqlRequestError h ("checkId", "Developer error!")

-- | checkUserThisNewsAuthor - Is he the author of this news?
checkUserThisNewsAuthor ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.User ->
  IdNews ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO IdNews
checkUserThisNewsAuthor conn h DataTypes.User {..} newsId = do
  res <-
    liftIO
      ( EXS.try $
          SQL.query
            conn
            [sql| SELECT news_author_login  FROM news WHERE news_id = ? |]
            (SQL.Only newsId) ::
          IO (Either SQL.SqlError [SQL.Only String])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("checkUserThisNewsAuthor", show err)
    Right [SQL.Only newsAuthorLogin] -> do
      if newsAuthorLogin == userLogin
        then do
          liftIO $ Logger.logDebug (News.hLogHandle h) "checkUserThisNewsAuthor: OK!"
          return newsId
        else do
          liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidPermissionAddEditNews (ErrorTypes.InvalidAuthorPermission "checkUserThisNewsAuthor: BAD! User is not this news author. Invalid Permission for this request."))
          EX.throwE $
            ErrorTypes.InvalidPermissionAddEditNews $
              ErrorTypes.InvalidAuthorPermission []
    Right _ -> Throw.throwSqlRequestError h ("checkUserThisNewsAuthor", "Developer error!")

checkImageFilesExist ::
  News.Handle IO ->
  DataTypes.EditNewsRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.EditNewsRequest
checkImageFilesExist _ r@DataTypes.EditNewsRequest {newImages = Nothing} =
  return r
checkImageFilesExist h r@DataTypes.EditNewsRequest {newImages = Just req} = do
  rez <- liftIO $ mapM (SD.doesFileExist . DataTypes.image) req
  if and rez
    then do
      liftIO $ Logger.logDebug (News.hLogHandle h) "checkImageFilesExist: OK!"
      return r
    else do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.NotExistImageFileAddEditNews (ErrorTypes.InvalidContent "checkImageFileExist: BAD!  File: does not exist (No such file or directory) "))
      EX.throwE $
        ErrorTypes.NotExistImageFileAddEditNews $ ErrorTypes.InvalidContent []

-- | checkCategoryId check if there is a record with a given category id in the database ( only for the case with an editable category)
checkCategoryId ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.EditNewsRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.EditNewsRequest
checkCategoryId _ _ r@DataTypes.EditNewsRequest {newCategoryId = Nothing} =
  return r
checkCategoryId conn h r@DataTypes.EditNewsRequest {newCategoryId = Just categoryId} = do
  res <-
    liftIO
      ( EXS.try $
          SQL.query
            conn
            [sql|SELECT EXISTS (SELECT category_path  FROM category WHERE category_id = ?)|]
            (SQL.Only categoryId) ::
          IO (Either SQL.SqlError [SQL.Only Bool])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("checkCategoryId", show err)
    Right [SQL.Only True] -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) "checkCategoryId: OK!  Image exist "
      return r
    Right [SQL.Only False] -> do
      liftIO $
        Logger.logError
          (News.hLogHandle h)
          ( "ERROR "
              .< ErrorTypes.InvalidCategoryIdAddEditNews
                ( ErrorTypes.InvalidContent "checkCategoryId: BAD! Not exists category with id "
                )
          )
      EX.throwE $ ErrorTypes.InvalidCategoryIdAddEditNews $ ErrorTypes.InvalidContent []
    Right _ -> Throw.throwSqlRequestError h ("checkId", "Developer error!")

checkPngImages ::
  Monad m =>
  News.Handle m ->
  DataTypes.EditNewsRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError m DataTypes.EditNewsRequest
checkPngImages _ r@DataTypes.EditNewsRequest {newImages = Nothing} =
  return r
checkPngImages h r@DataTypes.EditNewsRequest {newImages = Just req} =
  if length (filter (\x -> DataTypes.format x == "png") req) == length req
    then do
      lift $ Logger.logDebug (News.hLogHandle h) "checkPngImages: OK!"
      return r
    else do
      lift $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.NotPngImageAddEditNews (ErrorTypes.InvalidContent "checkPngImages: BAD!"))
      EX.throwE $
        ErrorTypes.NotPngImageAddEditNews $ ErrorTypes.InvalidContent []

checkBase64Images ::
  News.Handle IO ->
  DataTypes.EditNewsRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.EditNewsRequest
checkBase64Images _ r@DataTypes.EditNewsRequest {newImages = Nothing} =
  return r
checkBase64Images h r@DataTypes.EditNewsRequest {newImages = Just req} = do
  imageFiles <- liftIO $ mapM (B.readFile . DataTypes.image) req
  if length (filter Base64.isBase64 imageFiles) == length req
    then do
      liftIO $ Logger.logDebug (News.hLogHandle h) "checkBase64Image: OK!"
      return r
    else do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.NotBase64ImageAddEditNews (ErrorTypes.InvalidContent "checkBase64Image: BAD!"))
      EX.throwE $
        ErrorTypes.NotBase64ImageAddEditNews $ ErrorTypes.InvalidContent []

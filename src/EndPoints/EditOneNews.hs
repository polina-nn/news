module EndPoints.EditOneNews
  ( editOneNews,
    editNews,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Data.Time as TIME
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Database.PostgreSQL.Simple.Types as SQLTypes
import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.News.NewsIO as NewsIO
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
  DataTypes.User ->
  IdNews ->
  DataTypes.EditNewsRequest ->
  Handler DataTypes.News
editOneNews h DataTypes.Db {..} user catId r =
  (>>=) (liftIO $ dbEditNews (h, user, catId, r)) ToHttpResponse.toHttpResponse

editNews ::
  SQL.Connection ->
  (News.Handle IO, DataTypes.User, IdNews, DataTypes.EditNewsRequest) ->
  IO (Either ErrorTypes.AddEditNewsError DataTypes.News)
editNews conn (h, user, newsId, r) = do EX.runExceptT $ editNewsExcept conn (h, user, newsId, r)

editNewsExcept ::
  SQL.Connection ->
  (News.Handle IO, DataTypes.User, IdNews, DataTypes.EditNewsRequest) ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.News
editNewsExcept conn (h, user, newsId, r) = do
  liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["Request: Edit One News \n", ToText.toText r, "with news id ", T.pack $ show newsId, "\nby user: ", ToText.toText user]
  _ <- checkId conn h newsId >> checkUserThisNewsAuthor conn h user newsId >> checkImageFilesExist h r >>= checkCategoryId conn h >> checkPngImages h r >> checkBase64Images h r
  newsCategory <-
    newTitle conn h newsId r >> newCatId conn h newsId r >> newText conn h newsId r
      >> newImages conn h r
      >> newPublish conn h newsId r
      >> getNewsCategory conn h newsId
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
    liftIO $
      SQL.execute
        conn
        [sql| UPDATE news SET news_title = ? WHERE news_id = ? |]
        (title, newsId)
  case read (show res) :: Int of
    1 -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) "newTile: OK! UPDATE news "
      return r
    _ -> do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "newTile: BAD! Don't UPDATE  news"))
      EX.throwE $
        ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []

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
    liftIO $
      SQL.execute
        conn
        [sql| UPDATE news SET news_category_id = ? WHERE news_id = ? |]
        (catId, newsId)
  case read (show res) :: Int of
    1 -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) "newCatId: OK! UPDATE news "
      return r
    _ -> do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "newCatId: BAD! Don't UPDATE  news"))
      EX.throwE $
        ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []

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
    liftIO $
      SQL.execute
        conn
        [sql| UPDATE news SET news_text = ? WHERE news_id = ? |]
        (text, newsId)
  case read (show res) :: Int of
    1 -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) "newText: OK! UPDATE news "
      return r
    _ -> do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "newText: BAD! Don't UPDATE  news"))
      EX.throwE $
        ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []

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
    liftIO $
      SQL.execute
        conn
        [sql| UPDATE news SET news_published = ? WHERE news_id = ? |]
        (pub, newsId)
  case read (show res) :: Int of
    1 -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) "newPublish: OK! UPDATE news "
      return r
    _ -> do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "newPublish: BAD! Don't UPDATE  news"))
      EX.throwE $
        ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []

getNewsCategory ::
  SQL.Connection ->
  News.Handle IO ->
  IdNews ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO [DataTypes.Category]
getNewsCategory conn h idNews = do
  idCategory <- getNewsCategoryId conn h idNews
  categoryPath <- getCategoryPath conn h idCategory
  getCategories conn h categoryPath
  where
    getNewsCategoryId ::
      SQL.Connection ->
      News.Handle IO ->
      IdNews ->
      EX.ExceptT ErrorTypes.AddEditNewsError IO IdCategory
    getNewsCategoryId conn' h' idNews' = do
      res <-
        liftIO $
          SQL.query
            conn'
            [sql| SELECT news_category_id   FROM news WHERE  news_id = ? |]
            (SQL.Only idNews')
      case res of
        [val] -> do
          let categoryId = SQL.fromOnly val
          liftIO $ Logger.logDebug (News.hLogHandle h) ("getNewsCategoryId: OK! " .< categoryId)
          return categoryId
        _ -> do
          liftIO $ Logger.logError (News.hLogHandle h') ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "getNewsCategory: getNewsCategoryId : BAD!"))
          EX.throwE $
            ErrorTypes.AddEditNewsSQLRequestError $
              ErrorTypes.SQLRequestError []
    getCategoryPath ::
      SQL.Connection ->
      News.Handle IO ->
      IdCategory ->
      EX.ExceptT ErrorTypes.AddEditNewsError IO CategoryPath
    getCategoryPath conn'' h'' categoryId = do
      res <-
        liftIO $
          SQL.query
            conn''
            [sql| SELECT category_path  FROM category WHERE category_id = ?|]
            (SQL.Only categoryId)
      case res of
        [val] -> do
          let categoryPath = SQL.fromOnly val
          liftIO $ Logger.logDebug (News.hLogHandle h) ("getCategoryPath: OK! " .< categoryPath)
          return categoryPath
        _ -> do
          liftIO $ Logger.logError (News.hLogHandle h'') ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "getNewsCategory: getCategoryPath : BAD "))
          EX.throwE $
            ErrorTypes.AddEditNewsSQLRequestError $
              ErrorTypes.SQLRequestError []
    getCategories ::
      SQL.Connection ->
      News.Handle IO ->
      CategoryPath ->
      EX.ExceptT ErrorTypes.AddEditNewsError IO [DataTypes.Category]
    getCategories con h''' path = do
      res <-
        liftIO $
          SQL.query
            con
            [sql| SELECT category_path, category_id, category_name FROM category WHERE ? LIKE category_path||'%' ORDER BY category_path|]
            (SQL.Only path)
      case res of
        [] -> do
          liftIO $
            Logger.logError
              (News.hLogHandle h''')
              ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "getNewsCategory: getCategories : BAD "))
          EX.throwE $
            ErrorTypes.AddEditNewsSQLRequestError $
              ErrorTypes.SQLRequestError []
        _ -> do
          let categories = Prelude.map Category.toCategories res
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
      ( SQL.query
          conn
          [sql| SELECT EXISTS (SELECT news_images_id FROM news WHERE  news_id = ? AND news_images_id IS NOT NULL) |]
          (SQL.Only idNews)
      )
  case res of
    [val] ->
      if SQL.fromOnly val
        then
          ( do
              [ids] <-
                liftIO $
                  SQL.query
                    conn
                    [sql| SELECT news_images_id    FROM news WHERE  news_id = ? |]
                    (SQL.Only idNews)
              let idImages = SQLTypes.fromPGArray $ SQL.fromOnly ids
              return idImages
          )
        else return []
    _ -> do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "checkId! Don't checkId category"))
      EX.throwE $
        ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []

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
      ( SQL.query
          conn
          [sql| SELECT news_title , news_created, news_text,  news_published 
                FROM news 
                WHERE  news_id = ? |]
          (SQL.Only idNews) ::
          IO [(T.Text, TIME.Day, T.Text, Bool)]
      )
  case res of
    [val] -> do
      let news = toNews val user categories imagesIds idNews
      liftIO $ Logger.logDebug (News.hLogHandle h) $ T.concat ["addNews: OK!", ToText.toText news]
      return news
    _ -> do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "addNews! Don't INSERT INTO  news table"))
      EX.throwE $
        ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []
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
    liftIO $
      SQL.query
        conn
        [sql| SELECT EXISTS (SELECT news_id  FROM news WHERE news_id = ?) |]
        (SQL.Only newsId)
  case res of
    [] -> do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "checkId! Don't checkId category"))
      EX.throwE $
        ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []
    _ ->
      if SQL.fromOnly $ head res
        then
          ( do
              liftIO $ Logger.logDebug (News.hLogHandle h) ("checkId: OK! Exists news with id " .< newsId)
              return newsId
          )
        else
          ( do
              liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidNewsId (ErrorTypes.InvalidId "checkId: BAD! Not exists news with id "))
              EX.throwE $
                ErrorTypes.InvalidNewsId $ ErrorTypes.InvalidId []
          )

-- | checkUserThisNewsAuthor - Is he the author of this news?
checkUserThisNewsAuthor ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.User ->
  IdNews ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO IdNews
checkUserThisNewsAuthor conn h DataTypes.User {..} newsId = do
  res <-
    liftIO $
      SQL.query
        conn
        [sql| SELECT news_author_login  FROM news WHERE news_id = ? |]
        (SQL.Only newsId)
  case res of
    [newsAuthorLogin] ->
      if SQL.fromOnly newsAuthorLogin == userLogin
        then do
          liftIO $ Logger.logDebug (News.hLogHandle h) "checkUserThisNewsAuthor: OK!"
          return newsId
        else do
          liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidPermissionAddEditNews (ErrorTypes.InvalidAuthorPermission "checkUserThisNewsAuthor: BAD! User is not this news author. Invalid Permission for this request."))
          EX.throwE $
            ErrorTypes.InvalidPermissionAddEditNews $
              ErrorTypes.InvalidAuthorPermission []
    _ -> do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "checkUserThisNewsAuthor: BAD!"))
      EX.throwE $
        ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []

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
      ( SQL.query
          conn
          [sql| SELECT category_path  FROM category WHERE category_id = ?|]
          (SQL.Only categoryId) ::
          IO [SQL.Only String]
      )
  case res of
    [_] -> return r
    _ -> do
      liftIO $
        Logger.logError
          (News.hLogHandle h)
          ( "ERROR "
              .< ErrorTypes.InvalidCategoryIdAddEditNews
                ( ErrorTypes.InvalidContent "checkCategoryId: BAD! Not exists category with id "
                )
          )
      EX.throwE $
        ErrorTypes.InvalidCategoryIdAddEditNews $ ErrorTypes.InvalidContent []

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

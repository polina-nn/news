module EndPoints.AddOneNews
  ( addNews,
    addOneNews,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Int as I
import qualified Data.Pool as POOL
import qualified Data.Text as T
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

type IdImage = Int

addOneNews ::
  News.Handle IO ->
  DataTypes.Db ->
  DataTypes.Token ->
  DataTypes.CreateNewsRequest ->
  Handler DataTypes.News
addOneNews h DataTypes.Db {..} user createNewsReq =
  (>>=)
    (liftIO $ dbAddNews (h, user, createNewsReq))
    ToHttpResponse.toHttpResponse

addNews ::
  POOL.Pool SQL.Connection ->
  (News.Handle IO, DataTypes.Token, DataTypes.CreateNewsRequest) ->
  IO (Either ErrorTypes.AddEditNewsError DataTypes.News)
addNews pool (h, token, r) = EX.runExceptT $ addNewsExcept pool (h, token, r)

addNewsExcept ::
  POOL.Pool SQL.Connection ->
  (News.Handle IO, DataTypes.Token, DataTypes.CreateNewsRequest) ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.News
addNewsExcept pool (h, token, r) = do
  user <- EX.withExceptT ErrorTypes.AddEditNewsSQLRequestError (LibIO.searchUser h pool token)
  liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["Request: Add One News: \n", ToText.toText r, "by user: ", ToText.toText user]
  _ <- EX.withExceptT ErrorTypes.InvalidPermissionAddEditNews (Lib.checkUserAuthor h user)
  _ <- checkImageFilesExist h r
  _ <- checkPngImages h r
  _ <- checkBase64Images h r
  path <- checkCategoryId pool h r
  categories' <- getCategories pool h path
  newsId' <- getNewsId pool h
  images' <- addAllImages pool h r
  addNewsToDB pool h user categories' r newsId' images'

checkImageFilesExist ::
  News.Handle IO ->
  DataTypes.CreateNewsRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.CreateNewsRequest
checkImageFilesExist _ r@(DataTypes.CreateNewsRequest _ _ _ Nothing _) =
  return r
checkImageFilesExist h r@(DataTypes.CreateNewsRequest _ _ _ (Just req) _) = do
  rez <- liftIO $ mapM (SD.doesFileExist . DataTypes.image) req
  if and rez
    then do
      liftIO $ Logger.logDebug (News.hLogHandle h) "checkImageFilesExist: OK!"
      return r
    else do
      liftIO $
        Logger.logError
          (News.hLogHandle h)
          ( "ERROR "
              .< ErrorTypes.NotExistImageFileAddEditNews
                ( ErrorTypes.InvalidContent
                    "checkImageFileExist: BAD!  File: does not exist (No such file or directory) "
                )
          )
      EX.throwE $
        ErrorTypes.NotExistImageFileAddEditNews $ ErrorTypes.InvalidContent []

checkPngImages ::
  Monad m =>
  News.Handle m ->
  DataTypes.CreateNewsRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError m DataTypes.CreateNewsRequest
checkPngImages _ r@(DataTypes.CreateNewsRequest _ _ _ Nothing _) =
  return r
checkPngImages h r@(DataTypes.CreateNewsRequest _ _ _ (Just req) _) =
  if length (filter (\x -> DataTypes.format x == "png") req) == length req
    then do
      lift $ Logger.logDebug (News.hLogHandle h) "checkPngImages: OK!"
      return r
    else do
      lift $
        Logger.logError
          (News.hLogHandle h)
          ( "ERROR "
              .< ErrorTypes.NotPngImageAddEditNews
                ( ErrorTypes.InvalidContent "checkPngImages: BAD!"
                )
          )
      EX.throwE $
        ErrorTypes.NotPngImageAddEditNews $ ErrorTypes.InvalidContent []

checkBase64Images ::
  News.Handle IO ->
  DataTypes.CreateNewsRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.CreateNewsRequest
checkBase64Images _ r@(DataTypes.CreateNewsRequest _ _ _ Nothing _) =
  return r
checkBase64Images h r@(DataTypes.CreateNewsRequest _ _ _ (Just req) _) = do
  imageFiles <- liftIO $ mapM (B.readFile . DataTypes.image) req
  if length (filter Base64.isBase64 imageFiles) == length req
    then do
      liftIO $ Logger.logDebug (News.hLogHandle h) "checkBase64Image: OK!"
      return r
    else do
      liftIO $
        Logger.logError
          (News.hLogHandle h)
          ( "ERROR "
              .< ErrorTypes.NotBase64ImageAddEditNews
                ( ErrorTypes.InvalidContent "checkBase64Image: BAD!"
                )
          )
      EX.throwE $
        ErrorTypes.NotBase64ImageAddEditNews $ ErrorTypes.InvalidContent []

-- | checkCategory -- check if category with the id (from request) exists and return its path
checkCategoryId ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateNewsRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.Path
checkCategoryId pool h DataTypes.CreateNewsRequest {..} = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| SELECT category_path  FROM category WHERE category_id = ?|]
                (SQL.Only newsCategoryId) ::
                IO [SQL.Only String]
          ) ::
          IO (Either EXS.SomeException [SQL.Only String])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("checkCategoryId", show err)
    Right [SQL.Only path] -> do
      liftIO $ Logger.logInfo (News.hLogHandle h) "checkCategoryId: OK!"
      return path
    Right _ -> do
      liftIO $
        Logger.logError
          (News.hLogHandle h)
          ( "ERROR "
              .< ErrorTypes.InvalidCategoryIdAddEditNews
                ( ErrorTypes.InvalidContent
                    "checkCategoryId: BAD! Category not exists"
                )
          )
      EX.throwE $ ErrorTypes.InvalidCategoryIdAddEditNews $ ErrorTypes.InvalidContent []

-- | getCategories - get a list of categories from the root to this category
getCategories ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.Path ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO [DataTypes.Category]
getCategories pool h path = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| SELECT category_path, category_id, category_name FROM category WHERE ? LIKE category_path||'%' ORDER BY category_path |]
                (SQL.Only path)
          ) ::
          IO (Either EXS.SomeException [(DataTypes.Path, DataTypes.Id, DataTypes.Name)])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("getCategories", show err)
    Right value -> do
      let categories = Prelude.map Category.toCategories value
      return categories

-- getNewsIdIO  get id for new news
getNewsId ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO IdNews
getNewsId pool h = do
  resId <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query_
                conn
                [sql| select NEXTVAL('news_id_seq')|]
          ) ::
          IO (Either EXS.SomeException [SQL.Only IdNews])
      )
  case resId of
    Left err -> Throw.throwSqlRequestError h ("getNewsId", show err)
    Right [SQL.Only idNews] -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) ("getNewsId: OK! News_id is " .< idNews)
      return idNews
    Right _ -> Throw.throwSqlRequestError h ("getNewsId", "Developer error")

-- | addAllImagesIO Add all the pictures for the news.
addAllImages ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateNewsRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO [IdImage]
addAllImages _ _ (DataTypes.CreateNewsRequest _ _ _ Nothing _) = return []
addAllImages pool h (DataTypes.CreateNewsRequest _ _ _ (Just req) _) = do
  rez <- mapM (NewsIO.addImageNews pool h) req
  liftIO $ Logger.logDebug (News.hLogHandle h) "addAllImages: OK!"
  return rez

addNewsToDB ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.User ->
  [DataTypes.Category] ->
  DataTypes.CreateNewsRequest ->
  IdNews ->
  [IdImage] ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.News
addNewsToDB pool h DataTypes.User {..} categories DataTypes.CreateNewsRequest {..} idNews idImages = do
  let imageUris = map (Lib.imageIdToURI h) idImages
  created <- liftIO Lib.currentDay
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.execute
                conn
                [sql| INSERT INTO news (news_images_id, news_id, news_title , news_created, news_author_login, news_category_id, news_text,  news_published ) VALUES (?, ?, ?, ?, ?, ?,?,? ) |]
                ( SQLTypes.PGArray idImages,
                  idNews,
                  title,
                  show created,
                  userLogin,
                  newsCategoryId,
                  text,
                  published
                )
          ) ::
          IO (Either EXS.SomeException I.Int64)
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("addNewsToDB", show err)
    Right 1 -> do
      let news =
            DataTypes.News
              { newsTitle = title,
                newsCreated = created,
                newsAuthor = userName,
                newsCategory = categories,
                newsText = text,
                newsImages = imageUris,
                newsPublished = published,
                newsId = idNews
              }
      liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["addNewsToDB: OK!", ToText.toText news]
      return news
    Right _ -> Throw.throwSqlRequestError h ("addNewsToDB", "Developer error")

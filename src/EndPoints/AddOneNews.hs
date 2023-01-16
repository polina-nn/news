module EndPoints.AddOneNews
  ( addNews,
    addOneNews,
  )
where

import Control.Exception.Base
  ( Exception (displayException),
    SomeException (SomeException),
    catch,
    throwIO,
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Database.PostgreSQL.Simple.Types as SQLTypes
import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.News.NewsIO as NewsIO
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logDebug, logError, (.<))
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
  DataTypes.User ->
  DataTypes.CreateNewsRequest ->
  Handler DataTypes.News
addOneNews h DataTypes.Db {..} user createNewsReq =
  (>>=)
    (liftIO $ _addNews (h, user, createNewsReq))
    ToHttpResponse.toHttpResponse

addNews ::
  SQL.Connection ->
  (News.Handle IO, DataTypes.User, DataTypes.CreateNewsRequest) ->
  IO (Either ErrorTypes.AddEditNewsError DataTypes.News)
addNews conn (h, user, r) = do
  Logger.logDebug (News.hLogHandle h) $ T.pack "Request: Add One News"
  allCheck <-
    Lib.checkUserAuthor h user >>= checkImageFilesExistIO h r
      >>= checkCategoryIdIO conn h
      >>= checkPngImages h r
      >>= checkBase64ImagesIO h r
  case allCheck :: Either ErrorTypes.AddEditNewsError [DataTypes.Category] of
    Left err -> return $ Left err
    Right categories ->
      catch
        ( getNewsIdIO conn h >>= addAllImagesIO conn h r
            >>= addNewsIO conn h user categories r
        )
        handleError
  where
    handleError ::
      SomeException -> IO (Either ErrorTypes.AddEditNewsError DataTypes.News)
    handleError (SomeException e) = do
      let errMsg = displayException e
      Logger.logError
        (News.hLogHandle h)
        (T.pack ("addNews:handleError:" ++ show errMsg))
      throwIO e

-- getNewsIdIO  get id for new news
getNewsIdIO ::
  SQL.Connection ->
  News.Handle IO ->
  IO (Either ErrorTypes.AddEditNewsError IdNews)
getNewsIdIO conn h = do
  resId <-
    SQL.query_ conn [sql| select NEXTVAL('news_id_seq');|] :: IO [SQL.Only Int]
  case resId of
    [val] -> do
      let idNews = SQL.fromOnly val
      Logger.logDebug (News.hLogHandle h) $
        T.pack ("getNewsIdIO: OK! News_id is " ++ show idNews)
      return $ Right idNews
    _ -> do
      Logger.logError (News.hLogHandle h) $
        T.pack $
          show $
            ErrorTypes.AddEditNewsSQLRequestError $
              ErrorTypes.SQLRequestError "getNewsIdIO: BAD!"
      return $
        Left $
          ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []

-- addAllImagesIO Add all the pictures for the news. Return the number of added pictures
addAllImagesIO ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateNewsRequest ->
  Either ErrorTypes.AddEditNewsError IdNews ->
  IO (Either ErrorTypes.AddEditNewsError (IdNews, [IdImage]))
addAllImagesIO _ _ _ (Left err) = return $ Left err
addAllImagesIO _ _ (DataTypes.CreateNewsRequest _ _ _ Nothing _) (Right idNews) =
  return $ Right (idNews, [])
addAllImagesIO conn h (DataTypes.CreateNewsRequest _ _ _ (Just req) _) (Right idNews) = do
  rez <- mapM (NewsIO.addImageNewsIO conn h) req
  let rez' = map help rez
  if 0 `notElem` rez' -- so all drawings are added
    then do
      Logger.logDebug (News.hLogHandle h) $ T.pack "addAllImagesIO: OK!"
      return $ Right (idNews, rez')
    else do
      Logger.logError (News.hLogHandle h) $
        T.pack $
          show $
            ErrorTypes.AddEditNewsSQLRequestError $
              ErrorTypes.SQLRequestError
                ("addAllImagesIO:BAD! Don't add all images. Only" ++ show (length rez))
      return $
        Left $
          ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []
  where
    help :: Either ErrorTypes.AddEditNewsError IdImage -> IdImage
    help (Left _) = 0
    help (Right val) = val

addNewsIO ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.User ->
  [DataTypes.Category] ->
  DataTypes.CreateNewsRequest ->
  Either ErrorTypes.AddEditNewsError (IdNews, [IdImage]) ->
  IO (Either ErrorTypes.AddEditNewsError DataTypes.News)
addNewsIO _ _ _ _ _ (Left err) = return $ Left err
addNewsIO conn h DataTypes.User {..} categories DataTypes.CreateNewsRequest {..} (Right (idNews, idImages)) = do
  let imageUris = map (Lib.imageIdToURI h) idImages
  created <- Lib.currentDay
  res <-
    SQL.execute
      conn
      "INSERT INTO news (news_images_id, news_id, news_title , news_created, news_author_login, news_category_id, news_text,  news_published ) VALUES (?, ?, ?, ?, ?, ?,?,? )"
      ( SQLTypes.PGArray idImages,
        idNews,
        title,
        show created,
        userLogin,
        newsCategoryId,
        text,
        published
      )
  case read (show res) :: Int of
    1 -> do
      let news =
            DataTypes.News
              { newsTitle = title,
                newsCreated = created,
                newsAuthor = userName,
                newsCategory = categories,
                newsText = text,
                newsImages = imageUris,
                newsPublished = published
              }
      Logger.logDebug (News.hLogHandle h) ("addNewsIO: OK!" .< ToText.toText news)
      return $ Right news
    _ -> do
      Logger.logError (News.hLogHandle h) $
        T.pack $
          show $
            ErrorTypes.AddEditNewsSQLRequestError $
              ErrorTypes.SQLRequestError "addNewsIO! Don't INSERT INTO  news table"
      return $
        Left $
          ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []

checkImageFilesExistIO ::
  News.Handle IO ->
  DataTypes.CreateNewsRequest ->
  Either ErrorTypes.InvalidAuthorPermission DataTypes.User ->
  IO (Either ErrorTypes.AddEditNewsError DataTypes.CreateNewsRequest)
checkImageFilesExistIO _ _ (Left err) =
  return $ Left $ ErrorTypes.InvalidPermissionAddEditNews err
checkImageFilesExistIO _ r@(DataTypes.CreateNewsRequest _ _ _ Nothing _) (Right _) =
  return $ Right r
checkImageFilesExistIO h r@(DataTypes.CreateNewsRequest _ _ _ (Just req) _) (Right _) = do
  rez <- mapM (SD.doesFileExist . DataTypes.image) req
  if and rez
    then do
      Logger.logDebug (News.hLogHandle h) $ T.pack "checkImageFilesExist: OK!"
      return $ Right r
    else do
      Logger.logError (News.hLogHandle h) $
        T.pack $
          show $
            ErrorTypes.NotExistImageFileAddEditNews $
              ErrorTypes.InvalidContent
                "checkImageFileExist: BAD!  File: does not exist (No such file or directory) "
      return . Left $
        ErrorTypes.NotExistImageFileAddEditNews $ ErrorTypes.InvalidContent []

checkCategoryIdIO ::
  SQL.Connection ->
  News.Handle IO ->
  Either ErrorTypes.AddEditNewsError DataTypes.CreateNewsRequest ->
  IO (Either ErrorTypes.AddEditNewsError [DataTypes.Category])
checkCategoryIdIO _ _ (Left err) = return $ Left err
checkCategoryIdIO conn h (Right DataTypes.CreateNewsRequest {..}) = do
  res <-
    SQL.query
      conn
      [sql| SELECT category_path  FROM category WHERE category_id = ?|]
      (SQL.Only newsCategoryId) ::
      IO [SQL.Only String]
  case res of
    [val] -> do
      let path = SQL.fromOnly val
      result <-
        SQL.query
          conn
          [sql| SELECT category_path, category_id, category_name FROM category WHERE ? LIKE category_path||'%' ORDER BY category_path;|]
          (SQL.Only path)
      let categories = Prelude.map Category.toCategories result
      return $ Right categories
    _ -> do
      Logger.logError (News.hLogHandle h) $
        T.pack $
          show $
            ErrorTypes.InvalidCategoryIdAddEditNews $
              ErrorTypes.InvalidContent
                ( "checkCategoryIdIO: BAD! Category with id "
                    ++ show newsCategoryId
                    ++ " not exists"
                )
      return $
        Left $
          ErrorTypes.InvalidCategoryIdAddEditNews $ ErrorTypes.InvalidContent []

checkPngImages ::
  News.Handle IO ->
  DataTypes.CreateNewsRequest ->
  Either ErrorTypes.AddEditNewsError [DataTypes.Category] ->
  IO (Either ErrorTypes.AddEditNewsError [DataTypes.Category])
checkPngImages _ _ (Left err) = return $ Left err
checkPngImages _ (DataTypes.CreateNewsRequest _ _ _ Nothing _) (Right val) =
  return $ Right val
checkPngImages h (DataTypes.CreateNewsRequest _ _ _ (Just req) _) (Right val) =
  if length (filter (\x -> DataTypes.format x == "png") req) == length req
    then do
      Logger.logDebug (News.hLogHandle h) $ T.pack "checkPngImages: OK!"
      return $ Right val
    else do
      Logger.logError (News.hLogHandle h) $
        T.pack $
          show $
            ErrorTypes.NotPngImageAddEditNews $
              ErrorTypes.InvalidContent "checkPngImages: BAD!"
      return . Left $
        ErrorTypes.NotPngImageAddEditNews $ ErrorTypes.InvalidContent []

checkBase64ImagesIO ::
  News.Handle IO ->
  DataTypes.CreateNewsRequest ->
  Either ErrorTypes.AddEditNewsError [DataTypes.Category] ->
  IO (Either ErrorTypes.AddEditNewsError [DataTypes.Category])
checkBase64ImagesIO _ _ (Left err) = return $ Left err
checkBase64ImagesIO _ (DataTypes.CreateNewsRequest _ _ _ Nothing _) (Right val) =
  return (Right val)
checkBase64ImagesIO h (DataTypes.CreateNewsRequest _ _ _ (Just req) _) (Right val) = do
  imageFiles <- mapM (B.readFile . DataTypes.image) req
  if length (filter Base64.isBase64 imageFiles) == length req
    then do
      Logger.logDebug (News.hLogHandle h) $ T.pack "checkBase64Image: OK!"
      return (Right val)
    else do
      Logger.logError (News.hLogHandle h) $
        T.pack $
          show $
            ErrorTypes.NotBase64ImageAddEditNews $
              ErrorTypes.InvalidContent "checkBase64Image: BAD!"
      return . Left $
        ErrorTypes.NotBase64ImageAddEditNews $ ErrorTypes.InvalidContent []

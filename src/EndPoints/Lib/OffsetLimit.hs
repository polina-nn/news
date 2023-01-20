-- |  EndPoints.Lib.OffsetLimit
module EndPoints.Lib.OffsetLimit
  ( checkOffset,
    checkLimit,
    checkOffsetLimit,
    checkOffsetLimitNews,
  )
where

import qualified Data.Text as T
import Logger (logDebug, logError, (.<))
import qualified News
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

-- | checkOffset -- return limit of request result
checkOffset ::
  Monad m =>
  News.Handle m ->
  Maybe DataTypes.Offset ->
  m (Either ErrorTypes.InvalidOffset DataTypes.Offset)
checkOffset _ Nothing = return $ Right 0
checkOffset h (Just offset)
  | offset >= 0 = return $ Right offset
  | otherwise = do
    Logger.logError
      (News.hLogHandle h)
      ( "ERROR "
          .< ErrorTypes.InvalidOffset
            ( "checkOffset: Offset in request is a negative number. Offset = "
                ++ show offset
            )
      )
    return $ Left $ ErrorTypes.InvalidOffset []

-- | checkLimit -- return limit of request result
checkLimit ::
  Monad m =>
  News.Handle m ->
  -- | take from request for example in http://localhost:8080/users?limit=10
  Maybe DataTypes.Limit ->
  -- | take from  AppConfig
  Int ->
  -- | don`t show more then limit from AppConfig
  m (Either ErrorTypes.InvalidLimit DataTypes.Limit)
checkLimit _ Nothing appConfigLimit =
  return $ Right (appConfigLimit :: DataTypes.Limit)
checkLimit h (Just limit) appConfigLimit
  | limit > appConfigLimit = return $ Right (appConfigLimit :: DataTypes.Limit)
  | limit <= appConfigLimit && (limit > 0) = return $ Right limit
  | otherwise = do
    Logger.logError
      (News.hLogHandle h)
      ( "ERROR "
          .< ErrorTypes.InvalidLimit
            ( "checkLimit: Limit in request is a negative number or zero. Limit = "
                ++ show limit
            )
      )
    return $ Left $ ErrorTypes.InvalidLimit []

checkOffsetLimit ::
  Monad m =>
  News.Handle m ->
  Maybe DataTypes.Offset ->
  Maybe DataTypes.Limit ->
  m (Either ErrorTypes.GetContentError (DataTypes.Offset, DataTypes.Limit))
checkOffsetLimit h mo ml = do
  checkOff <- checkOffset h mo
  case checkOff of
    Left err -> return $ Left $ ErrorTypes.InvalidOffsetGetContent err
    Right offset -> do
      checkLim <- checkLimit h ml (News.appShowLimit $ News.hAppConfig h)
      case checkLim of
        Left err -> return $ Left $ ErrorTypes.InvalidLimitGetContent err
        Right limit -> do
          Logger.logDebug (News.hLogHandle h) $ T.concat ["checkOffsetLimit: OK! Offset = ", T.pack $ show offset, "Limit  = ", T.pack $ show limit]
          return $ Right (offset, limit)

checkOffsetLimitNews ::
  Monad m =>
  News.Handle m ->
  Maybe DataTypes.Offset ->
  Maybe DataTypes.Limit ->
  m (Either ErrorTypes.GetNewsError (DataTypes.Offset, DataTypes.Limit))
checkOffsetLimitNews h mo ml = do
  Logger.logDebug (News.hLogHandle h) ("In request: offset = " .< mo)
  Logger.logDebug (News.hLogHandle h) ("In request: limit = " .< ml)
  checkOff <- checkOffset h mo
  case checkOff of
    Left err -> return $ Left $ ErrorTypes.InvalidOffsetGetNews err
    Right offset -> do
      checkLim <- checkLimit h ml (News.appShowLimit $ News.hAppConfig h)
      case checkLim of
        Left err -> return $ Left $ ErrorTypes.InvalidLimitGetNews err
        Right limit -> do
          Logger.logDebug (News.hLogHandle h) ("checkOffsetLimit: OK! Offset = " .< offset)
          Logger.logDebug (News.hLogHandle h) ("checkOffsetLimit: OK! Limit  = " .< limit)
          return $ Right (offset, limit)

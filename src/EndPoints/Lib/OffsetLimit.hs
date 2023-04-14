-- |  EndPoints.Lib.OffsetLimit
module EndPoints.Lib.OffsetLimit
  ( checkOffset,
    checkLimit,
    checkOffsetLimit,
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Control.Monad.Trans.Except as EX
import Logger (logDebug, logError, (.<))
import qualified News
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

-- | checkOffset -- return limit of request result
checkOffset ::
  Monad m =>
  News.Handle m ->
  Maybe DataTypes.Offset ->
  EX.ExceptT ErrorTypes.InvalidOffsetOrLimit m DataTypes.Offset
checkOffset _ Nothing = return DataTypes.Offset {offset = 0}
checkOffset h (Just r@DataTypes.Offset {..})
  | offset >= 0 = return r
  | otherwise = do
    lift $
      Logger.logError
        (News.hLogHandle h)
        ("ERROR " .< ErrorTypes.InvalidOffset "checkOffset: Offset in request is a negative number.")
    EX.throwE $ ErrorTypes.InvalidOffset []

-- | checkLimit -- return limit of request result
checkLimit ::
  Monad m =>
  News.Handle m ->
  -- | take from request for example in http://localhost:8080/users?limit=10
  Maybe DataTypes.Limit ->
  -- | take from  AppConfig
  Int ->
  -- | don`t show more then limit from AppConfig
  EX.ExceptT ErrorTypes.InvalidOffsetOrLimit m DataTypes.Limit
checkLimit _ Nothing appConfigLimit =
  return DataTypes.Limit {limit = appConfigLimit}
checkLimit h (Just r@DataTypes.Limit {..}) appConfigLimit
  | limit > appConfigLimit = return DataTypes.Limit {limit = appConfigLimit}
  | limit <= appConfigLimit && (limit > 0) = return r
  | otherwise = do
    lift $
      Logger.logError
        (News.hLogHandle h)
        ("ERROR " .< ErrorTypes.InvalidLimit "checkLimit: Limit in request is a negative number or zero.")
    EX.throwE $ ErrorTypes.InvalidLimit []

checkOffsetLimit ::
  Monad m =>
  News.Handle m ->
  Maybe DataTypes.Offset ->
  Maybe DataTypes.Limit ->
  EX.ExceptT ErrorTypes.InvalidOffsetOrLimit m (DataTypes.Offset, DataTypes.Limit)
checkOffsetLimit h mo ml = do
  offset' <- checkOffset h mo
  limit' <- checkLimit h ml (News.appShowLimit $ News.hAppConfig h)
  lift $ Logger.logDebug (News.hLogHandle h) $ "checkOffsetLimit: OK! Offset = " .< DataTypes.offset offset' <> ", Limit  = " .< DataTypes.limit limit'
  return (offset', limit')

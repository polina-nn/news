--{-# LANGUAGE OverloadedStrings #-}

module OffsetLimitSpec
  ( spec,
  )
where

import Control.Monad.Identity (Identity (Identity))
import qualified EndPoints.Lib.OffsetLimit as OffsetLimit
import Handle (appConfigSpec, appConfigSpecForLimit, appShowLimit, handleSpec, handleSpecForLimit)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck
  ( Negative (Negative),
    NonNegative (NonNegative),
    NonPositive (NonPositive),
    Property,
    Testable (property),
    (===),
    (==>),
  )
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

spec :: Spec
spec =
  describe "OffsetLimit - logic for getting results of request " $ do
    -- checkOffset tests
    it "checkOffset: returns the (Right 0) if offset is not set in request" $
      OffsetLimit.checkOffset handleSpec Nothing === Identity (Right 0)
    it "checkOffset: returns the (Right offset) if offset is >= 0 " $
      property $ \(NonNegative count) -> OffsetLimit.checkOffset handleSpec (Just count) === Identity (Right count)
    it "checkOffset: returns the (Left InvalidOffset) if offset < 0 " $
      property $ \(Negative count) -> OffsetLimit.checkOffset handleSpec (Just count) === Identity (Left $ ErrorTypes.InvalidOffset [])
    -- realLimit tests
    it "checkLimit: returns the (Right appConfigLimit) if limit is not set in request" $
      OffsetLimit.checkLimit handleSpecForLimit Nothing (appShowLimit appConfigSpec) === Identity (Right (appShowLimit appConfigSpec :: DataTypes.Limit))
    it "checkLimit: returns the (Right appConfigLimit) if  Limit > appConfigLimit (property limitMoreThenConfig)" $
      property limitMoreThenConfig
    it "checkLimit: returns the (Left InvalidLimit ) if  Limit <= 0 " $
      property $ \(NonPositive limit) -> OffsetLimit.checkLimit handleSpecForLimit (Just limit) (appShowLimit appConfigSpec) === Identity (Left $ ErrorTypes.InvalidLimit [])
    it "checkLimit: returns the (Right limit) if  0 < Limit <= appConfigLimit (property limitRight)" $
      property limitRight
    -- checkOffsetLimit
    it "checkOffsetLimit: returns the Right (offset, limit): case1" $
      property validOffsetAndLimit1
    it "checkOffsetLimit: returns the Right (offset, limit): case2" $
      property validOffsetAndLimit2
    it "checkOffsetLimit: returns the Right (offset, limit): case3" $
      property validOffsetAndLimit3
    it "checkOffsetLimit: returns the Right (offset, limit): case4" $
      property validOffsetAndLimit4
    it "checkOffsetLimit: returns the Right (offset, limit): case5" $
      property $ \(NonNegative offset) -> OffsetLimit.checkOffsetLimit handleSpec (Just offset) Nothing === Identity (Right (offset, appShowLimit appConfigSpec))
    it "checkOffsetLimit: returns the Right (offset, limit): case6" $
      OffsetLimit.checkOffsetLimit handleSpec Nothing Nothing === Identity (Right (0, appShowLimit appConfigSpec))
    it "checkOffsetLimit: returns the Left (offsetErr, limitErr) " $
      property invalidOffsetAndLimit
    it "checkOffsetLimit: returns the Left (offsetErr, Nothing) case1" $
      property invalidOffset1
    it "checkOffsetLimit: returns the Left (offsetErr, Nothing) case2" $
      property invalidOffset2
    it "checkOffsetLimit: returns the Left (Nothing, limitErr) case1 " $
      property invalidLimit1
    it "checkOffsetLimit: returns the Left (Nothing, limitErr) case2 " $
      property invalidLimit2

limitMoreThenConfig :: Int -> Property
limitMoreThenConfig limit = (limit > appShowLimit appConfigSpec) ==> OffsetLimit.checkLimit handleSpecForLimit (Just limit) (appShowLimit appConfigSpec) === Identity (Right (appShowLimit appConfigSpec :: DataTypes.Limit))

limitRight :: Int -> Property
limitRight limit = ((limit <= appShowLimit appConfigSpecForLimit) && (limit > 0)) ==> OffsetLimit.checkLimit handleSpecForLimit (Just limit) (appShowLimit appConfigSpecForLimit) === Identity (Right limit)

validOffsetAndLimit1 :: Int -> Int -> Property
validOffsetAndLimit1 offset limit = (offset >= 0 && limit > 0 && limit <= appShowLimit appConfigSpecForLimit) ==> OffsetLimit.checkOffsetLimit handleSpecForLimit (Just offset) (Just limit) === Identity (Right (offset, limit))

validOffsetAndLimit2 :: Int -> Int -> Property
validOffsetAndLimit2 offset limit = (offset >= 0 && limit > appShowLimit appConfigSpec) ==> OffsetLimit.checkOffsetLimit handleSpec (Just offset) (Just limit) === Identity (Right (offset, appShowLimit appConfigSpec))

validOffsetAndLimit3 :: Int -> Property
validOffsetAndLimit3 limit = (limit > 0 && limit <= appShowLimit appConfigSpecForLimit) ==> OffsetLimit.checkOffsetLimit handleSpecForLimit Nothing (Just limit) === Identity (Right (0, limit))

validOffsetAndLimit4 :: Int -> Property
validOffsetAndLimit4 limit = (limit > appShowLimit appConfigSpec) ==> OffsetLimit.checkOffsetLimit handleSpec (Nothing) (Just limit) === Identity (Right (0 :: DataTypes.Offset, appShowLimit appConfigSpec))

invalidOffsetAndLimit :: Int -> Int -> Property
invalidOffsetAndLimit offset limit =
  (offset < 0 && limit <= 0) ==> OffsetLimit.checkOffsetLimit handleSpec (Just offset) (Just limit)
    === Identity (Left (ErrorTypes.InvalidOffsetGetContent $ ErrorTypes.InvalidOffset []))

invalidOffset1 :: Int -> Int -> Property
invalidOffset1 offset limit =
  offset < 0 && limit > 0 ==> OffsetLimit.checkOffsetLimit handleSpec (Just offset) (Just limit)
    === Identity (Left (ErrorTypes.InvalidOffsetGetContent $ ErrorTypes.InvalidOffset []))

invalidOffset2 :: Int -> Property
invalidOffset2 offset =
  offset < 0 ==> OffsetLimit.checkOffsetLimit handleSpec (Just offset) (Nothing)
    === Identity (Left (ErrorTypes.InvalidOffsetGetContent $ ErrorTypes.InvalidOffset []))

invalidLimit1 :: Int -> Int -> Property
invalidLimit1 offset limit =
  (offset >= 0 && limit < 0) ==> OffsetLimit.checkOffsetLimit (handleSpec) (Just offset) (Just limit)
    === Identity (Left (ErrorTypes.InvalidLimitGetContent $ ErrorTypes.InvalidLimit []))

invalidLimit2 :: Int -> Property
invalidLimit2 limit =
  limit < 0 ==> OffsetLimit.checkOffsetLimit handleSpec Nothing (Just limit)
    === Identity (Left (ErrorTypes.InvalidLimitGetContent $ ErrorTypes.InvalidLimit []))

{--
spec :: Spec
spec =
  describe "OffsetLimit - logic for getting results of request " $ do
    -- realOffset tests
    it "realOffset: returns the (Right 0) if offset is not set in request" $
      OffsetLimit.realOffset Nothing === Right 0
    it "realOffset: returns the (Right offset) if offset is >= 0 " $
      property $ \(NonNegative count) -> OffsetLimit.realOffset (Just count) === Right count
    it "realOffset: returns the (Left InvalidOffset) if offset < 0 " $
      property $ \(Negative count) -> OffsetLimit.realOffset (Just count) === Left ErrorTypes.InvalidOffset {ErrorTypes.msgOffsetError = "realOffset: Offset in requst is a negative number. Offset = " ++ show count} -- если оффсет меньше  0
      -- realLimit tests
    it "realLimit: returns the (Right appConfigLimit) if limit is not set in request" $
      OffsetLimit.realLimit Nothing (appShowLimit appConfigSpec) === Right (appShowLimit appConfigSpec :: DataTypes.Limit)
    it "realLimit: returns the (Right appConfigLimit) if  Limit > appConfigLimit" $
      property limitMoreThenConfig
    it "realLimit: returns the (Left InvalidLimit ) if  Limit <= 0 " $
      property $ \(NonPositive limit) -> OffsetLimit.realLimit (Just limit) (appShowLimit appConfigSpec) === Left ErrorTypes.InvalidLimit {ErrorTypes.msgLimitError = "realLimit: Limit in requst is a negative number or zero. Limit = " ++ show limit}
    it "realLimit: returns the (Right limit) if  0 < Limit <= appConfigLimit " $
      property limitRight
    -- checkOffsetLimit
    it "checkOffsetLimit: returns the Right (offset, limit): case1" $
      property validOffsetAndLimit1
    it "checkOffsetLimit: returns the Right (offset, limit): case2" $
      property validOffsetAndLimit2
    it "checkOffsetLimit: returns the Right (offset, limit): case3" $
      property validOffsetAndLimit3
    it "checkOffsetLimit: returns the Right (offset, limit): case4" $
      property validOffsetAndLimit4
    it "checkOffsetLimit: returns the Right (offset, limit): case5" $
      property $ \(NonNegative offset) -> OffsetLimit.checkOffsetLimit (handleSpec, Just offset, Nothing) === Identity (Right (offset, appShowLimit appConfigSpec))
    it "checkOffsetLimit: returns the Right (offset, limit): case6" $
      OffsetLimit.checkOffsetLimit (handleSpec, Nothing, Nothing) === Identity (Right (0, appShowLimit appConfigSpec))
    it "checkOffsetLimit: returns the Left (offsetErr, limitErr) " $
      property invalidOffsetAndLimit
    it "checkOffsetLimit: returns the Left (offsetErr, Nothing) case1" $
      property invalidOffset1
    it "checkOffsetLimit: returns the Left (offsetErr, Nothing) case2" $
      property invalidOffset2
    it "checkOffsetLimit: returns the Left (Nothing, limitErr) case1 " $
      property invalidLimit1
    it "checkOffsetLimit: returns the Left (Nothing, limitErr) case2 " $
      property invalidLimit2

limitMoreThenConfig :: Int -> Property
limitMoreThenConfig limit = (limit > appShowLimit appConfigSpec) ==> OffsetLimit.realLimit (Just limit) (appShowLimit appConfigSpec) === Right (appShowLimit appConfigSpec :: DataTypes.Limit)

limitRight :: Int -> Property
limitRight limit = ((limit <= appShowLimit appConfigSpecForLimit) && (limit > 0)) ==> OffsetLimit.realLimit (Just limit) (appShowLimit appConfigSpecForLimit) === Right limit

validOffsetAndLimit1 :: Int -> Int -> Property
validOffsetAndLimit1 offset limit = (offset >= 0 && limit > 0 && limit <= appShowLimit appConfigSpecForLimit) ==> OffsetLimit.checkOffsetLimit (handleSpecForLimit, Just offset, Just limit) === Identity (Right (offset, limit))

validOffsetAndLimit2 :: Int -> Int -> Property
validOffsetAndLimit2 offset limit = (offset >= 0 && limit > appShowLimit appConfigSpec) ==> OffsetLimit.checkOffsetLimit (handleSpec, Just offset, Just limit) === Identity (Right (offset, appShowLimit appConfigSpec))

validOffsetAndLimit3 :: Int -> Property
validOffsetAndLimit3 limit = (limit > 0 && limit <= appShowLimit appConfigSpecForLimit) ==> OffsetLimit.checkOffsetLimit (handleSpecForLimit, Nothing, Just limit) === Identity (Right (0, limit))

validOffsetAndLimit4 :: Int -> Property
validOffsetAndLimit4 limit = (limit > appShowLimit appConfigSpec) ==> OffsetLimit.checkOffsetLimit (handleSpec, Nothing, Just limit) === Identity (Right (0 :: DataTypes.Offset, appShowLimit appConfigSpec))

invalidOffsetAndLimit :: Int -> Int -> Property
invalidOffsetAndLimit offset limit =
  (offset < 0 && limit <= 0) ==> OffsetLimit.checkOffsetLimit (handleSpec, Just offset, Just limit)
    === Identity (Left (ErrorTypes.OffsetLimitError offsetError limitError))
  where
    offsetError :: Maybe ErrorTypes.InvalidOffset
    offsetError = Just (ErrorTypes.InvalidOffset {ErrorTypes.msgOffsetError = "realOffset: Offset in requst is a negative number. Offset = " ++ show offset})
    limitError :: Maybe ErrorTypes.InvalidLimit
    limitError = Just (ErrorTypes.InvalidLimit {ErrorTypes.msgLimitError = "realLimit: Limit in requst is a negative number or zero. Limit = " ++ show limit})

invalidOffset1 :: Int -> Int -> Property
invalidOffset1 offset limit =
  offset < 0 && limit > 0 ==> OffsetLimit.checkOffsetLimit (handleSpec, Just offset, Just limit)
    === Identity (Left (ErrorTypes.OffsetLimitError offsetError limitError))
  where
    offsetError :: Maybe ErrorTypes.InvalidOffset
    offsetError = Just (ErrorTypes.InvalidOffset {ErrorTypes.msgOffsetError = "realOffset: Offset in requst is a negative number. Offset = " ++ show offset})
    limitError :: Maybe ErrorTypes.InvalidLimit
    limitError = Nothing

invalidOffset2 :: Int -> Property
invalidOffset2 offset =
  offset < 0 ==> OffsetLimit.checkOffsetLimit (handleSpec, Just offset, Nothing)
    === Identity (Left (ErrorTypes.OffsetLimitError offsetError limitError))
  where
    offsetError :: Maybe ErrorTypes.InvalidOffset
    offsetError = Just (ErrorTypes.InvalidOffset {ErrorTypes.msgOffsetError = "realOffset: Offset in requst is a negative number. Offset = " ++ show offset})
    limitError :: Maybe ErrorTypes.InvalidLimit
    limitError = Nothing

invalidLimit1 :: Int -> Int -> Property
invalidLimit1 offset limit =
  (offset >= 0 && limit < 0) ==> OffsetLimit.checkOffsetLimit (handleSpec, Just offset, Just limit)
    === Identity (Left (ErrorTypes.OffsetLimitError offsetError limitError))
  where
    offsetError :: Maybe ErrorTypes.InvalidOffset
    offsetError = Nothing
    limitError :: Maybe ErrorTypes.InvalidLimit
    limitError = Just (ErrorTypes.InvalidLimit {ErrorTypes.msgLimitError = "realLimit: Limit in requst is a negative number or zero. Limit = " ++ show limit})

invalidLimit2 :: Int -> Property
invalidLimit2 limit =
  limit < 0 ==> OffsetLimit.checkOffsetLimit (handleSpec, Nothing, Just limit)
    === Identity (Left (ErrorTypes.OffsetLimitError offsetError limitError))
  where
    offsetError :: Maybe ErrorTypes.InvalidOffset
    offsetError = Nothing
    limitError :: Maybe ErrorTypes.InvalidLimit
    limitError = Just (ErrorTypes.InvalidLimit {ErrorTypes.msgLimitError = "realLimit: Limit in requst is a negative number or zero. Limit = " ++ show limit})

{-# LANGUAGE OverloadedStrings #-}

module EndPoints.OffsetLimit where

--( checkOffsetLimit,
--  realOffset, -- use only in tests
--  realLimit, -- use only in tests
-- )

import qualified Data.Text as T
import qualified Logger
import qualified News
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

-- | realOffset -- return offset for request result
realOffset :: Maybe DataTypes.Offset -> Either ErrorTypes.InvalidOffset DataTypes.Offset
realOffset Nothing = Right 0
realOffset (Just offset)
  | offset >= 0 = Right offset
  | otherwise = Left ErrorTypes.InvalidOffset {ErrorTypes.msgOffsetError = "realOffset: Offset in requst is a negative number. Offset = " ++ show offset}

-- | realLimit -- return limit of request result
realLimit ::
  -- | take from requst for example in http://localhost:8080/users?limit=10
  Maybe DataTypes.Limit ->
  -- | take from  AppConfig
  Int ->
  -- | don`t show more then limit from AppConfig
  Either ErrorTypes.InvalidLimit DataTypes.Limit
realLimit Nothing appConfigLimit = Right (appConfigLimit :: DataTypes.Limit)
realLimit (Just limit) appConfigLimit
  | limit > appConfigLimit = Right (appConfigLimit :: DataTypes.Limit)
  | limit <= appConfigLimit && (limit > 0) = Right limit
  | otherwise = Left ErrorTypes.InvalidLimit {ErrorTypes.msgLimitError = "realLimit: Limit in requst is a negative number or zero. Limit = " ++ show limit}

--полиморфный вариант для тестирования
checkOffsetLimit :: Monad m => (News.Handle m, Maybe DataTypes.Offset, Maybe DataTypes.Limit) -> m (Either ErrorTypes.OffsetLimitError (DataTypes.Offset, DataTypes.Limit))
checkOffsetLimit (h, mo, ml) = do helpCheckOffsetLimit h (realOffset mo) (realLimit ml (News.appShowLimit $ News.hAppConfig h))
  where
    helpCheckOffsetLimit :: Monad m => News.Handle m -> Either ErrorTypes.InvalidOffset DataTypes.Offset -> Either ErrorTypes.InvalidLimit DataTypes.Limit -> m (Either ErrorTypes.OffsetLimitError (DataTypes.Offset, DataTypes.Limit))
    helpCheckOffsetLimit h' (Left offsetErr) (Left limitErr) = do
      Logger.logError (News.hLogHandle h') $ T.pack ("checkOffsetLimit: BAD! \n" ++ show offsetErr ++ "\n" ++ show limitErr)
      return $ Left ErrorTypes.OffsetLimitError {ErrorTypes.offsetError = Just offsetErr, ErrorTypes.limitError = Just limitErr}
    helpCheckOffsetLimit h' (Right offset) (Right limit) = do
      Logger.logDebug (News.hLogHandle h') $ T.pack ("checkOffsetLimit: OK! offset= " ++ show offset ++ "\n limit = " ++ show limit)
      return $ Right (offset, limit)
    helpCheckOffsetLimit h' (Right offset) (Left limitErr) = do
      Logger.logError (News.hLogHandle h') $ T.pack ("checkOffsetLimit: BAD! \n" ++ show limitErr ++ " \n offset = " ++ show offset)
      return $ Left ErrorTypes.OffsetLimitError {ErrorTypes.offsetError = Nothing, ErrorTypes.limitError = Just limitErr}
    helpCheckOffsetLimit h' (Left offsetErr) (Right limit) = do
      Logger.logError (News.hLogHandle h') $ T.pack ("checkOffsetLimit: BAD! \n" ++ show offsetErr ++ " \n limit = " ++ show limit)
      return $ Left ErrorTypes.OffsetLimitError {ErrorTypes.offsetError = Just offsetErr, ErrorTypes.limitError = Nothing}
--}
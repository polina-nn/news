module OffsetLimitSpec
  ( spec,
  )
where

import Control.Monad.Identity (Identity (Identity))
import qualified EndPoints.Lib.OffsetLimit as OffsetLimit
import Handle
  ( appConfigSpec,
    appConfigSpecForLimit,
    appShowLimit,
    handleSpec,
    handleSpecForLimit,
  )
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
  describe "OffsetLimit - logic for getting results of request " $
    -- checkOffset tests
    do
      it "checkOffset: returns the (Right 0) if offset is not set in request" $
        OffsetLimit.checkOffset handleSpec Nothing === Identity (Right 0)
      it "checkOffset: returns the (Right offset) if offset is >= 0 " $
        property $ \(NonNegative count) ->
          OffsetLimit.checkOffset handleSpec (Just count)
            === Identity (Right count)
      it "checkOffset: returns the (Left InvalidOffset) if offset < 0 " $
        property $ \(Negative count) ->
          OffsetLimit.checkOffset handleSpec (Just count)
            === Identity (Left $ ErrorTypes.InvalidOffset [])
      -- realLimit tests
      it
        "checkLimit: returns the (Right appConfigLimit) if limit is not set in request"
        $ OffsetLimit.checkLimit
          handleSpecForLimit
          Nothing
          (appShowLimit appConfigSpec)
          === Identity (Right (appShowLimit appConfigSpec :: DataTypes.Limit))
      it
        "checkLimit: returns the (Right appConfigLimit) if  Limit > appConfigLimit (property limitMoreThenConfig)"
        $ property limitMoreThenConfig
      it "checkLimit: returns the (Left InvalidLimit ) if  Limit <= 0 " $
        property $ \(NonPositive limit) ->
          OffsetLimit.checkLimit
            handleSpecForLimit
            (Just limit)
            (appShowLimit appConfigSpec)
            === Identity (Left $ ErrorTypes.InvalidLimit [])
      it
        "checkLimit: returns the (Right limit) if  0 < Limit <= appConfigLimit (property limitRight)"
        $ property limitRight
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
        property $ \(NonNegative offset) ->
          OffsetLimit.checkOffsetLimit handleSpec (Just offset) Nothing
            === Identity (Right (offset, appShowLimit appConfigSpec))
      it "checkOffsetLimit: returns the Right (offset, limit): case6" $
        OffsetLimit.checkOffsetLimit handleSpec Nothing Nothing
          === Identity (Right (0, appShowLimit appConfigSpec))
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
limitMoreThenConfig limit =
  (limit > appShowLimit appConfigSpec)
    ==> OffsetLimit.checkLimit
      handleSpecForLimit
      (Just limit)
      (appShowLimit appConfigSpec)
    === Identity (Right (appShowLimit appConfigSpec :: DataTypes.Limit))

limitRight :: Int -> Property
limitRight limit =
  ((limit <= appShowLimit appConfigSpecForLimit) && (limit > 0))
    ==> OffsetLimit.checkLimit
      handleSpecForLimit
      (Just limit)
      (appShowLimit appConfigSpecForLimit)
    === Identity (Right limit)

validOffsetAndLimit1 :: Int -> Int -> Property
validOffsetAndLimit1 offset limit =
  (offset >= 0 && limit > 0 && limit <= appShowLimit appConfigSpecForLimit)
    ==> OffsetLimit.checkOffsetLimit handleSpecForLimit (Just offset) (Just limit)
    === Identity (Right (offset, limit))

validOffsetAndLimit2 :: Int -> Int -> Property
validOffsetAndLimit2 offset limit =
  (offset >= 0 && limit > appShowLimit appConfigSpec)
    ==> OffsetLimit.checkOffsetLimit handleSpec (Just offset) (Just limit)
    === Identity (Right (offset, appShowLimit appConfigSpec))

validOffsetAndLimit3 :: Int -> Property
validOffsetAndLimit3 limit =
  (limit > 0 && limit <= appShowLimit appConfigSpecForLimit)
    ==> OffsetLimit.checkOffsetLimit handleSpecForLimit Nothing (Just limit)
    === Identity (Right (0, limit))

validOffsetAndLimit4 :: Int -> Property
validOffsetAndLimit4 limit =
  (limit > appShowLimit appConfigSpec)
    ==> OffsetLimit.checkOffsetLimit handleSpec Nothing (Just limit)
    === Identity (Right (0 :: DataTypes.Offset, appShowLimit appConfigSpec))

invalidOffsetAndLimit :: Int -> Int -> Property
invalidOffsetAndLimit offset limit =
  (offset < 0 && limit <= 0)
    ==> OffsetLimit.checkOffsetLimit handleSpec (Just offset) (Just limit)
    === Identity
      (Left (ErrorTypes.InvalidOffsetGetContent $ ErrorTypes.InvalidOffset []))

invalidOffset1 :: Int -> Int -> Property
invalidOffset1 offset limit =
  offset < 0
    && limit
    > 0 ==> OffsetLimit.checkOffsetLimit handleSpec (Just offset) (Just limit)
    === Identity
      (Left (ErrorTypes.InvalidOffsetGetContent $ ErrorTypes.InvalidOffset []))

invalidOffset2 :: Int -> Property
invalidOffset2 offset =
  offset
    < 0 ==> OffsetLimit.checkOffsetLimit handleSpec (Just offset) Nothing
    === Identity
      (Left (ErrorTypes.InvalidOffsetGetContent $ ErrorTypes.InvalidOffset []))

invalidLimit1 :: Int -> Int -> Property
invalidLimit1 offset limit =
  (offset >= 0 && limit < 0)
    ==> OffsetLimit.checkOffsetLimit handleSpec (Just offset) (Just limit)
    === Identity
      (Left (ErrorTypes.InvalidLimitGetContent $ ErrorTypes.InvalidLimit []))

invalidLimit2 :: Int -> Property
invalidLimit2 limit =
  limit
    < 0 ==> OffsetLimit.checkOffsetLimit handleSpec Nothing (Just limit)
    === Identity
      (Left (ErrorTypes.InvalidLimitGetContent $ ErrorTypes.InvalidLimit []))

module OffsetLimitSpec
  ( spec,
  )
where

import qualified Control.Monad.Trans.Except as EX
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
        OffsetLimit.checkOffset handleSpec Nothing === EX.except (Right (DataTypes.Offset {offset = 0}))
      it "checkOffset: returns the (Right offset) if offset is >= 0 " $
        property $ \(NonNegative count) ->
          OffsetLimit.checkOffset handleSpec (Just (DataTypes.Offset {offset = count}))
            === EX.except (Right (DataTypes.Offset {offset = count}))
      it "checkOffset: returns the (Left InvalidOffset) if offset < 0 " $
        property $ \(Negative count) ->
          OffsetLimit.checkOffset handleSpec (Just (DataTypes.Offset {offset = count}))
            === EX.throwE (ErrorTypes.InvalidOffset [])
      -- realLimit tests
      it
        "checkLimit: returns the (Right appConfigLimit) if limit is not set in request"
        $ OffsetLimit.checkLimit
          handleSpecForLimit
          Nothing
          (appShowLimit appConfigSpec)
          === EX.except (Right (DataTypes.Limit {limit = appShowLimit appConfigSpec}))
      it
        "checkLimit: returns the (Right appConfigLimit) if  Limit > appConfigLimit (property limitMoreThenConfig)"
        $ property limitMoreThenConfig
      it "checkLimit: returns the (Left InvalidLimit ) if  Limit <= 0 " $
        property $ \(NonPositive limit') ->
          OffsetLimit.checkLimit
            handleSpecForLimit
            (Just (DataTypes.Limit {limit = limit'}))
            (appShowLimit appConfigSpec)
            === EX.throwE (ErrorTypes.InvalidLimit [])
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
        property $ \(NonNegative offset') ->
          OffsetLimit.checkOffsetLimit handleSpec (Just (DataTypes.Offset {offset = offset'})) Nothing
            === EX.except (Right (DataTypes.Offset {offset = offset'}, DataTypes.Limit {limit = appShowLimit appConfigSpec}))
      it "checkOffsetLimit: returns the Right (offset, limit): case6" $
        OffsetLimit.checkOffsetLimit handleSpec Nothing Nothing
          === EX.except (Right (DataTypes.Offset {offset = 0}, DataTypes.Limit {limit = appShowLimit appConfigSpec}))
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
limitMoreThenConfig limit' =
  (limit' > appShowLimit appConfigSpec)
    ==> OffsetLimit.checkLimit
      handleSpecForLimit
      (Just (DataTypes.Limit {limit = limit'}))
      (appShowLimit appConfigSpec)
    === EX.except (Right (DataTypes.Limit {limit = appShowLimit appConfigSpec}))

limitRight :: Int -> Property
limitRight limit' =
  ((limit' <= appShowLimit appConfigSpecForLimit) && (limit' > 0))
    ==> OffsetLimit.checkLimit
      handleSpecForLimit
      (Just DataTypes.Limit {limit = limit'})
      (appShowLimit appConfigSpecForLimit)
    === EX.except (Right DataTypes.Limit {limit = limit'})

validOffsetAndLimit1 :: Int -> Int -> Property
validOffsetAndLimit1 offset' limit' =
  (offset' >= 0 && limit' > 0 && limit' <= appShowLimit appConfigSpecForLimit)
    ==> OffsetLimit.checkOffsetLimit handleSpecForLimit (Just DataTypes.Offset {offset = offset'}) (Just DataTypes.Limit {limit = limit'})
    === EX.except (Right (DataTypes.Offset {offset = offset'}, DataTypes.Limit {limit = limit'}))

validOffsetAndLimit2 :: Int -> Int -> Property
validOffsetAndLimit2 offset' limit' =
  (offset' >= 0 && limit' > appShowLimit appConfigSpec)
    ==> OffsetLimit.checkOffsetLimit handleSpec (Just DataTypes.Offset {offset = offset'}) (Just DataTypes.Limit {limit = limit'})
    === EX.except (Right (DataTypes.Offset {offset = offset'}, DataTypes.Limit {limit = appShowLimit appConfigSpec}))

validOffsetAndLimit3 :: Int -> Property
validOffsetAndLimit3 limit' =
  (limit' > 0 && limit' <= appShowLimit appConfigSpecForLimit)
    ==> OffsetLimit.checkOffsetLimit handleSpecForLimit Nothing (Just DataTypes.Limit {limit = limit'})
    === EX.except (Right (DataTypes.Offset {offset = 0}, DataTypes.Limit {limit = limit'}))

validOffsetAndLimit4 :: Int -> Property
validOffsetAndLimit4 limit' =
  (limit' > appShowLimit appConfigSpec)
    ==> OffsetLimit.checkOffsetLimit handleSpec Nothing (Just DataTypes.Limit {limit = limit'})
    === EX.except (Right (DataTypes.Offset {offset = 0}, DataTypes.Limit {limit = appShowLimit appConfigSpec}))

invalidOffsetAndLimit :: Int -> Int -> Property
invalidOffsetAndLimit offset' limit' =
  (offset' < 0 && limit' <= 0)
    ==> OffsetLimit.checkOffsetLimit handleSpec (Just DataTypes.Offset {offset = offset'}) (Just DataTypes.Limit {limit = limit'})
    === EX.throwE (ErrorTypes.InvalidOffset [])

invalidOffset1 :: Int -> Int -> Property
invalidOffset1 offset' limit' =
  offset' < 0
    && limit'
    > 0 ==> OffsetLimit.checkOffsetLimit handleSpec (Just DataTypes.Offset {offset = offset'}) (Just DataTypes.Limit {limit = limit'})
    === EX.throwE (ErrorTypes.InvalidOffset [])

invalidOffset2 :: Int -> Property
invalidOffset2 offset' =
  offset'
    < 0 ==> OffsetLimit.checkOffsetLimit handleSpec (Just DataTypes.Offset {offset = offset'}) Nothing
    === EX.throwE (ErrorTypes.InvalidOffset [])

invalidLimit1 :: Int -> Int -> Property
invalidLimit1 offset' limit' =
  (offset' >= 0 && limit' < 0)
    ==> OffsetLimit.checkOffsetLimit handleSpec (Just DataTypes.Offset {offset = offset'}) (Just DataTypes.Limit {limit = limit'})
    === EX.throwE (ErrorTypes.InvalidLimit [])

invalidLimit2 :: Int -> Property
invalidLimit2 limit' =
  limit' < 0
    ==> OffsetLimit.checkOffsetLimit handleSpec Nothing (Just DataTypes.Limit {limit = limit'})
    === EX.throwE (ErrorTypes.InvalidLimit [])

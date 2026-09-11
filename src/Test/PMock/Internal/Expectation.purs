module Test.PMock.Internal.Expectation
  ( Expectation(..)
  , Expectations
  , Verification
  , TimesSpec
  , class With
  , atLeast
  , atMost
  , greaterThan
  , lessThan
  , called
  , calledWith
  , calledInOrder
  , calledInPartialOrder
  , calledInSequence
  , anything
  , inOrderWith
  , inPartialOrderWith
  , never
  , once
  , times
  , with
  , verifyVerification
  , verifyExpectations
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (snoc, unsnoc)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Exception (Error)
import Partial.Unsafe (unsafeCrashWith)
import Test.PMock.Internal.Param (class MatchParams, MatchParamsOps, matchParamsOps)
import Test.PMock.Internal.Types (Mock)
import Test.PMock.Internal.Verify
  ( CountVerifyMethod(..)
  , VerifyOrderMethod(..)
  , class ToDirectExpected
  , _verifyCountBy
  , _verifyOrderBy
  , _verifyTotalCount
  , toDirectExpected
  )

data Expectation params
  = CountAny CountVerifyMethod
  | CountWith (MatchParamsOps params) CountVerifyMethod params
  | Order (MatchParamsOps params) VerifyOrderMethod (Array params)

newtype Expectations params a = Expectations
  { values :: Array (Expectation params)
  , result :: a
  }

instance functorExpectations :: Functor (Expectations params) where
  map f (Expectations definition) = Expectations
    { values: definition.values
    , result: f definition.result
    }

instance applyExpectations :: Apply (Expectations params) where
  apply (Expectations functions) (Expectations values) = Expectations
    { values: functions.values <> values.values
    , result: functions.result values.result
    }

instance applicativeExpectations :: Applicative (Expectations params) where
  pure result = Expectations { values: [], result }

instance bindExpectations :: Bind (Expectations params) where
  bind (Expectations definition) next =
    let Expectations nextDefinition = next definition.result
    in Expectations
      { values: definition.values <> nextDefinition.values
      , result: nextDefinition.result
      }

instance monadExpectations :: Monad (Expectations params)

newtype Verification params = Verification (Expectation params)

newtype TimesSpec = TimesSpec CountVerifyMethod

called :: forall params. TimesSpec -> Expectations params Unit
called (TimesSpec method) = Expectations
  { values: [ CountAny method ]
  , result: unit
  }

calledWith
  :: forall input params
   . ToDirectExpected input params
  => MatchParams params
  => input
  -> Verification params
calledWith input =
  Verification $ CountWith matchParamsOps (GreaterThanEqual 1) $ toDirectExpected input

anything :: forall params. Verification params
anything = Verification $ CountAny (GreaterThanEqual 1)

once :: TimesSpec
once = TimesSpec (Equal 1)

never :: TimesSpec
never = TimesSpec (Equal 0)

times :: Int -> TimesSpec
times = TimesSpec <<< Equal

atLeast :: Int -> TimesSpec
atLeast = TimesSpec <<< GreaterThanEqual

atMost :: Int -> TimesSpec
atMost = TimesSpec <<< LessThanEqual

greaterThan :: Int -> TimesSpec
greaterThan = TimesSpec <<< GreaterThan

lessThan :: Int -> TimesSpec
lessThan = TimesSpec <<< LessThan

calledInOrder
  :: forall input params
   . ToDirectExpected input params
  => MatchParams params
  => Array input
  -> Expectations params Unit
calledInOrder inputs = Expectations
  { values: [ Order matchParamsOps ExactlySequence (toDirectExpected <$> inputs) ]
  , result: unit
  }

calledInPartialOrder
  :: forall input params
   . ToDirectExpected input params
  => MatchParams params
  => Array input
  -> Expectations params Unit
calledInPartialOrder inputs = Expectations
  { values: [ Order matchParamsOps PartiallySequence (toDirectExpected <$> inputs) ]
  , result: unit
  }

calledInSequence
  :: forall input params
   . ToDirectExpected input params
  => MatchParams params
  => Array input
  -> Expectations params Unit
calledInSequence = calledInPartialOrder

inOrderWith
  :: forall input params
   . ToDirectExpected input params
  => MatchParams params
  => Array input
  -> Verification params
inOrderWith inputs =
  Verification $ Order matchParamsOps ExactlySequence (toDirectExpected <$> inputs)

inPartialOrderWith
  :: forall input params
   . ToDirectExpected input params
  => MatchParams params
  => Array input
  -> Verification params
inPartialOrderWith inputs =
  Verification $ Order matchParamsOps PartiallySequence (toDirectExpected <$> inputs)

class With specification input result | specification input -> result where
  with :: specification -> input -> result

instance withExpectations ::
  ( ToDirectExpected input params
  , MatchParams params
  ) => With (Expectations params Unit) input (Expectations params Unit) where
  with (Expectations definition) input =
    case unsnoc definition.values of
      Just { init, last: CountAny method } -> Expectations
        { values: snoc init (CountWith matchParamsOps method $ toDirectExpected input)
        , result: unit
        }
      _ -> unsafeCrashWith "with: no count-only expectation to add arguments to"

instance withCount ::
  ( ToDirectExpected input params
  , MatchParams params
  ) => With TimesSpec input (Verification params) where
  with (TimesSpec method) input =
    Verification $ CountWith matchParamsOps method $ toDirectExpected input

verifyVerification
  :: forall fun params m
   . MonadThrow Error m
  => Mock fun params
  -> Verification params
  -> m Unit
verifyVerification built (Verification expectation) =
  verifyExpectation built expectation

verifyExpectation
  :: forall fun params m
   . MonadThrow Error m
  => Mock fun params
  -> Expectation params
  -> m Unit
verifyExpectation built expectation = case expectation of
  CountAny method -> _verifyTotalCount built method
  CountWith operations method params -> _verifyCountBy operations built params method
  Order operations method params -> _verifyOrderBy operations method built params

verifyExpectations
  :: forall fun params m
   . MonadThrow Error m
  => Mock fun params
  -> Expectations params Unit
  -> m Unit
verifyExpectations mock (Expectations definition) =
  traverse_ (verifyExpectation mock) definition.values

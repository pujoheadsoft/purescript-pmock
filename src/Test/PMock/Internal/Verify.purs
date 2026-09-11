module Test.PMock.Internal.Verify
  ( CountVerifyMethod(..)
  , VerifyMatchType(..)
  , VerifyOrderMethod(..)
  , class Verify
  , class VerifyCount
  , class VerifyOrder
  , class ToDirectExpected
  , class ToExpectedParams
  , toDirectExpected
  , toExpectedParams
  , showCalledParams
  , verify
  , verifyCount
  , verifyPartiallySequence
  , verifySequence
  , _verifyCount
  , _verifyCountBy
  , _verifyOrder
  , _verifyOrderBy
  , _verifyTotalCount
  , hasBeenCalledWith
  , hasFunctionBeenCalledTimes
  , hasFunctionBeenCalledWith
  , hasNotBeenCalledWith
  , hasBeenCalledTimes
  , hasBeenCalledTimesGreaterThanEqual
  , hasBeenCalledTimesLessThanEqual
  , hasBeenCalledTimesGreaterThan
  , hasBeenCalledTimesLessThan
  , hasBeenRunTimes
  , with
  , hasBeenCalledInOrder
  , hasBeenCalledInPartialOrder
  ) where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array (catMaybes, filter, length, mapWithIndex, (!!))
import Data.Array as A
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), isNothing)
import Data.String (joinWith)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error)
import Partial.Unsafe (unsafeCrashWith)
import Test.PMock.Internal.Message
  ( countMismatchMessage
  , detailedArgumentMismatchMessage
  , detailedCountMismatchMessage
  , exactOrderCountMismatchMessage
  , messageFromRendered
  , orderMismatchMessage
  , partialOrderCountMismatchMessage
  , partialOrderMismatchMessage
  , verificationFailureMessage
  )
import Test.PMock.Internal.Cons (Cons(..))
import Test.PMock.Internal.Param
  ( class MatchParams
  , MatchParamsOps
  , Param
  , Result(..)
  , matchesParams
  , matchParamsOps
  , param
  , renderActualParams
  , renderExpectedParams
  )
import Test.PMock.Internal.Registry (lookupRecorder)
import Test.PMock.Internal.Types (CalledParamsList, Mock(..), MockName, Verifier(..), VerifyFailed(..))

fail :: forall a m. MonadThrow Error m => String -> m a
fail = throwError <<< error

data VerifyMatchType a = MatchAny a | MatchAll a

class Verify params input where
  verify :: forall fun m. MonadThrow Error m => Mock fun params -> input -> m Unit

instance instanceVerifyParamType :: Verify (Param a) (VerifyMatchType (Param a)) where
  verify v a = _verify v a
else
instance instanceVerifyParam :: (Eq a, Show a) => Verify (Param a) a where
  verify v a = _verify v (MatchAny (param a))
else
instance instanceVerifyType :: MatchParams a => Verify a (VerifyMatchType a) where
  verify v a = _verify v a
else
instance instanceVerifyConverted :: (ToExpectedParams input params, MatchParams params) => Verify params input where
  verify v input = _verify v (MatchAny (toExpectedParams input))
else
instance instanceVerify :: MatchParams a => Verify a a where
  verify v a = _verify v (MatchAny a)

hasBeenCalledWith
  :: forall @params @input fun m
   . Verify params input
  => MonadThrow Error m
  => Mock fun params
  -> input
  -> m Unit
hasBeenCalledWith = verify

hasFunctionBeenCalledWith
  :: forall fn params input m
   . ToDirectExpected input params
  => MatchParams params
  => MonadEffect m
  => MonadThrow Error m
  => fn
  -> input
  -> m Unit
hasFunctionBeenCalledWith fn input = do
  registered <- liftEffect $ lookupRecorder fn
  case registered of
    Just (registeredMock :: Mock fn params) ->
      _verify registeredMock (MatchAny $ toDirectExpected input)
    Nothing -> fail verificationFailureMessage

hasFunctionBeenCalledTimes
  :: forall fn params input m
   . ToDirectExpected input params
  => MatchParams params
  => MonadEffect m
  => MonadThrow Error m
  => fn
  -> Int
  -> input
  -> m Unit
hasFunctionBeenCalledTimes fn count input = do
  registered <- liftEffect $ lookupRecorder fn
  case registered of
    Just (registeredMock :: Mock fn params) ->
      _verifyCount registeredMock (toDirectExpected input) (Equal count)
    Nothing -> fail verificationFailureMessage

_verify :: forall fun params m. MatchParams params => MonadThrow Error m => Mock fun params -> VerifyMatchType params -> m Unit
_verify (Mock name _ (Verifier calledParamsList)) matcher =
  case doVerify name calledParamsList matcher of
    Just (VerifyFailed msg) -> fail msg
    Nothing -> pure unit

doVerify :: forall a. MatchParams a => Maybe MockName -> CalledParamsList a -> VerifyMatchType a -> Maybe VerifyFailed
doVerify name list (MatchAny a) = do
  guard $ A.all (not <<< matchesParams a) list
  pure $ verifyFailedMesssage name list a
doVerify name list (MatchAll a) = do
  guard $ A.any (not <<< matchesParams a) list
  pure $ verifyFailedMesssage name list a

verifyFailedMesssage :: forall a. MatchParams a => Maybe MockName -> CalledParamsList a -> a -> VerifyFailed
verifyFailedMesssage name calledParams expected =
  VerifyFailed $ messageFromRendered name
    (renderExpectedParams expected)
    (formatCalledParamsList expected calledParams)

formatCalledParamsList :: forall a. MatchParams a => a -> CalledParamsList a -> String
formatCalledParamsList expected calledParams
  | length calledParams == 0 =
    "Never been called."
  | length calledParams == 1 =
    renderActualParams expected (unsafeIndex calledParams 0)
  | otherwise = "[" <> joinWith "," (renderActualParams expected <$> calledParams) <> "]"

data CountVerifyMethod =
    Equal Int
  | LessThanEqual Int
  | GreaterThanEqual Int
  | LessThan Int
  | GreaterThan Int

compareCount :: CountVerifyMethod -> Int -> Boolean
compareCount (Equal e) a            = a == e
compareCount (LessThanEqual e) a    = a <= e
compareCount (LessThan e) a         = a <  e
compareCount (GreaterThanEqual e) a = a >= e
compareCount (GreaterThan e) a      = a >  e

instance showCountVerifyMethod :: Show CountVerifyMethod where
  show (Equal e)            = show e
  show (LessThanEqual e)    = "<= " <> show e
  show (LessThan e)         = "< " <> show e
  show (GreaterThanEqual e) = ">= " <> show e
  show (GreaterThan e)      = "> " <> show e

class VerifyCount countType params a where
  verifyCount :: forall fun m. MonadThrow Error m => MatchParams params => Mock fun params -> countType -> a -> m Unit

instance instanceVerifyCount3 :: (Eq a, Show a) => VerifyCount CountVerifyMethod (Param a) a where
  verifyCount v count a = _verifyCount v (param a) count
else
instance instanceVerifyCountParam1 :: (Eq a, Show a) => VerifyCount Int (Param a) a where
  verifyCount v count a =  _verifyCount v (param a) (Equal count)
else
instance instanceVerifyCountConvertedMethod ::
  (ToExpectedParams input params, MatchParams params) =>
  VerifyCount CountVerifyMethod params input where
  verifyCount v count input = _verifyCount v (toExpectedParams input) count
else
instance instanceVerifyCountConvertedInt ::
  (ToExpectedParams input params, MatchParams params) =>
  VerifyCount Int params input where
  verifyCount v count input = _verifyCount v (toExpectedParams input) (Equal count)
else
instance instanceVerifyCount2 :: VerifyCount CountVerifyMethod a a where
  verifyCount v count a = _verifyCount v a count
else
instance instanceVerifyCount :: VerifyCount Int a a where
  verifyCount v count a = _verifyCount v a (Equal count)

_verifyCount :: forall fun params m. MonadThrow Error m => MatchParams params => Mock fun params -> params -> CountVerifyMethod -> m Unit
_verifyCount = _verifyCountBy matchParamsOps

_verifyCountBy :: forall fun params m. MonadThrow Error m => MatchParamsOps params -> Mock fun params -> params -> CountVerifyMethod -> m Unit
_verifyCountBy operations (Mock name _ (Verifier calledParamsList)) v method =
  let
    callCount = length (filter (operations.matches v) calledParamsList)
  in if compareCount method callCount then pure unit
    else if callCount == 0 && length calledParamsList > 0 && expectsPositive method then
      fail $ detailedArgumentMismatchMessage name
        (operations.renderExpected v)
        (operations.renderActual v <$> calledParamsList)
    else fail $ detailedCountMismatchMessage name
      (operations.renderExpected v)
      (show method)
      callCount
      (calledParamsList <#> \called ->
        { actual: operations.renderActual v called
        , matches: operations.matches v called
        })

expectsPositive :: CountVerifyMethod -> Boolean
expectsPositive (Equal expected) = expected > 0
expectsPositive (GreaterThanEqual expected) = expected > 0
expectsPositive (GreaterThan _) = true
expectsPositive (LessThan _) = false
expectsPositive (LessThanEqual _) = false

_verifyTotalCount
  :: forall fun params m
   . MonadThrow Error m
  => Mock fun params
  -> CountVerifyMethod
  -> m Unit
_verifyTotalCount (Mock name _ (Verifier calledParamsList)) method =
  let callCount = length calledParamsList
  in if compareCount method callCount then pure unit
    else fail $ countMismatchMessage name (show method) callCount

hasBeenCalledTimes
  :: forall @countType @params @a fun m
   . VerifyCount countType params a
  => MonadThrow Error m
  => MatchParams params
  => Mock fun params
  -> countType
  -> a
  -> m Unit
hasBeenCalledTimes = verifyCount

hasBeenCalledTimesGreaterThanEqual
  :: forall params a fun m
   . VerifyCount CountVerifyMethod params a
  => MonadThrow Error m
  => MatchParams params
  => Mock fun params
  -> Int
  -> a
  -> m Unit
hasBeenCalledTimesGreaterThanEqual m i = hasBeenCalledTimes m (GreaterThanEqual i)

hasBeenCalledTimesLessThanEqual
  :: forall params a fun m
   . VerifyCount CountVerifyMethod params a
  => MonadThrow Error m
  => MatchParams params
  => Mock fun params
  -> Int
  -> a
  -> m Unit
hasBeenCalledTimesLessThanEqual m i = hasBeenCalledTimes m (LessThanEqual i)

hasBeenCalledTimesGreaterThan
  :: forall params a fun m
   . VerifyCount CountVerifyMethod params a
  => MonadThrow Error m
  => MatchParams params
  => Mock fun params
  -> Int
  -> a
  -> m Unit
hasBeenCalledTimesGreaterThan m i = hasBeenCalledTimes m (GreaterThan i)

hasBeenCalledTimesLessThan
  :: forall params a fun m
   . VerifyCount CountVerifyMethod params a
  => MonadThrow Error m
  => MatchParams params
  => Mock fun params
  -> Int
  -> a
  -> m Unit
hasBeenCalledTimesLessThan m i = hasBeenCalledTimes m (LessThan i)

hasBeenRunTimes
  :: forall @countType r m
   . VerifyCount countType (Param Unit) Unit
  => MonadThrow Error m
  => Mock (Effect r) (Param Unit)
  -> countType
  -> m Unit
hasBeenRunTimes m count = verifyCount m count unit

with :: forall a m. MonadThrow Error m => (a -> m Unit) -> a -> m Unit
with f a = f a

hasNotBeenCalledWith
  :: forall params a fun m
   . VerifyCount Int params a
  => MonadThrow Error m
  => MatchParams params
  => Mock fun params
  -> a
  -> m Unit
hasNotBeenCalledWith v a = verifyCount v 0 a

showCalledParams :: forall fun params. Show params => Mock fun params -> String
showCalledParams (Mock _ _ (Verifier calledParamsList)) = show calledParamsList

class VerifyOrder params input where
  verifySequence :: forall fun m. MonadThrow Error m => Mock fun params -> Array input -> m Unit
  verifyPartiallySequence :: forall fun m. MonadThrow Error m => Mock fun params -> Array input -> m Unit

instance instanceVerifyParamOrder :: (Eq a, Show a) => VerifyOrder (Param a) a where
  verifySequence v a = _verifyOrder ExactlySequence v $ param <$> a
  verifyPartiallySequence v a = _verifyOrder PartiallySequence v $ param <$> a
else
instance instanceVerifyConvertedOrder ::
  (ToExpectedParams input params, MatchParams params) => VerifyOrder params input where
  verifySequence v input = _verifyOrder ExactlySequence v $ toExpectedParams <$> input
  verifyPartiallySequence v input = _verifyOrder PartiallySequence v $ toExpectedParams <$> input
else
instance instanceVerifyOrder :: MatchParams a => VerifyOrder a a where
  verifySequence v a = _verifyOrder ExactlySequence v a
  verifyPartiallySequence v a = _verifyOrder PartiallySequence v a

hasBeenCalledInOrder
  :: forall @params @input fun m
   . VerifyOrder params input
  => MonadThrow Error m
  => Mock fun params
  -> Array input
  -> m Unit
hasBeenCalledInOrder = verifySequence

hasBeenCalledInPartialOrder
  :: forall @params @input fun m
   . VerifyOrder params input
  => MonadThrow Error m
  => Mock fun params
  -> Array input
  -> m Unit
hasBeenCalledInPartialOrder = verifyPartiallySequence

_verifyOrder
  :: forall fun params m
   . MatchParams params
  => MonadThrow Error m
  => VerifyOrderMethod
  -> Mock fun params
  -> Array params
  -> m Unit
_verifyOrder = _verifyOrderBy matchParamsOps

_verifyOrderBy
  :: forall fun params m
   . MonadThrow Error m
  => MatchParamsOps params
  -> VerifyOrderMethod
  -> Mock fun params
  -> Array params
  -> m Unit
_verifyOrderBy operations method (Mock name _ (Verifier calledParamsList)) matchers =
  case doVerifyOrder operations method name calledParamsList matchers of
    Just (VerifyFailed msg) -> fail msg
    Nothing -> pure unit

data VerifyOrderMethod
  = ExactlySequence
  | PartiallySequence

doVerifyOrder :: forall a. MatchParamsOps a -> VerifyOrderMethod -> Maybe MockName -> CalledParamsList a -> Array a -> Maybe VerifyFailed
doVerifyOrder operations ExactlySequence name calledValues expectedValues
  | length calledValues /= length expectedValues = do
    pure $ VerifyFailed $ exactOrderCountMismatchMessage name
      (length calledValues) (length expectedValues)
  | otherwise = do
    let unexpectedOrders = collectUnExpectedOrder operations calledValues expectedValues
    guard $ length unexpectedOrders > 0
    pure $ verifyFailedSequence operations name unexpectedOrders

doVerifyOrder operations PartiallySequence name calledValues expectedValues
  | length calledValues < length expectedValues = do
    pure $ VerifyFailed $ partialOrderCountMismatchMessage name
      (length calledValues) (length expectedValues)
  | otherwise = do
    guard $ isOrderNotMatched operations calledValues expectedValues
    pure $ verifyFailedPartiallySequence operations name calledValues expectedValues

type VerifyOrderResult a = {
  index :: Int,
  calledValue :: a,
  expectedValue :: a
}

collectUnExpectedOrder :: forall a. MatchParamsOps a -> CalledParamsList a -> Array a -> Array (VerifyOrderResult a)
collectUnExpectedOrder operations calledValues expectedValues =
  catMaybes $ mapWithIndex (\i expectedValue -> do
    let calledValue = unsafeIndex calledValues i
    guard $ not $ operations.matches expectedValue calledValue
    pure {index: i, calledValue, expectedValue}
  ) expectedValues

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex arr idx =
  case arr !! idx of
    Just a -> a
    Nothing -> unsafeCrashWith "Array is too short"

verifyFailedPartiallySequence :: forall a. MatchParamsOps a -> Maybe MockName -> CalledParamsList a -> Array a -> VerifyFailed
verifyFailedPartiallySequence operations name calledValues expectedValues =
  VerifyFailed $ partialOrderMismatchMessage name
    (operations.renderExpected <$> expectedValues)
    (renderCalledValues operations expectedValues calledValues)

isOrderNotMatched :: forall a. MatchParamsOps a -> CalledParamsList a -> Array a -> Boolean
isOrderNotMatched operations calledValues expectedValues =
  isNothing $ foldl (\candidates e -> do
    candidates >>= \c -> do
      index <- A.findIndex (operations.matches e) c
      Just $ A.drop (index + 1) c
  )
  (Just calledValues) expectedValues

verifyFailedSequence :: forall a. MatchParamsOps a -> Maybe MockName -> Array (VerifyOrderResult a) -> VerifyFailed
verifyFailedSequence operations name fails =
  VerifyFailed $ orderMismatchMessage name $ renderDifference <$> fails
  where
  renderDifference { index, calledValue, expectedValue } =
    { position: index + 1
    , expected: operations.renderExpected expectedValue
    , actual: operations.renderActual expectedValue calledValue
    }

renderCalledValues :: forall a. MatchParamsOps a -> Array a -> Array a -> Array String
renderCalledValues operations expectedValues calledValues =
  mapWithIndex renderCalled calledValues
  where
  renderCalled index calledValue = case expectedValues !! index of
    Just expectedValue -> operations.renderActual expectedValue calledValue
    Nothing -> case expectedValues !! 0 of
      Just expectedValue -> operations.renderActual expectedValue calledValue
      Nothing -> "<actual value>"

class ToDirectExpected input params | input -> params where
  toDirectExpected :: input -> params

instance toDirectExpectedParam :: ToDirectExpected (Param a) (Param a) where
  toDirectExpected = identity

else
instance toDirectExpectedCons :: ToExpectedParams (Cons head tail) params =>
  ToDirectExpected (Cons head tail) params where
  toDirectExpected = toExpectedParams

else
instance toDirectExpectedValue :: (Eq a, Show a) =>
  ToDirectExpected a (Param a) where
  toDirectExpected = param

class ToExpectedParams input params | input -> params where
  toExpectedParams :: input -> params

instance toExpectedParamsParam :: ToExpectedParams (Param a) (Param a) where
  toExpectedParams = identity

else
instance toExpectedParamsLast :: (Eq b, Show b) =>
  ToExpectedParams (Cons (Param a) (Result b)) (Cons (Param a) (Param b)) where
  toExpectedParams (Cons head (Result tail)) = Cons head (param tail)

else
instance toExpectedParamsMore :: ToExpectedParams tailInput tailParams =>
  ToExpectedParams (Cons (Param a) tailInput) (Cons (Param a) tailParams) where
  toExpectedParams (Cons head tail) = Cons head (toExpectedParams tail)

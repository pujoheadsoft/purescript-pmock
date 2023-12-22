module Test.PMock
  ( CountVerifyMethod(..)
  , Mock
  , VerifyMatchType(..)
  , class MockBuilder
  , class Verify
  , class VerifyCount
  , class VerifyOrder
  , fun
  , mock
  , namedMock
  , build
  , mockFun
  , namedMockFun
  , module Test.PMock.Cons
  , module Test.PMock.Param
  , showCalledParams
  , verify
  , verifyCount
  , verifyPartiallySequence
  , verifySequence
  , hasBeenCalledWith
  , hasNotBeenCalledWith
  , hasBeenCalledTimes
  , hasBeenCalledTimesGreaterThanEqual
  , hasBeenCalledTimesLessThanEqual
  , hasBeenCalledTimesGreaterThan
  , hasBeenCalledTimesLessThan
  , with
  , hasBeenCalledInOrder
  , hasBeenCalledInPartialOrder
  )
  where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (catMaybes, filter, find, length, mapWithIndex, (!!))
import Data.Array as A
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String (joinWith)
import Data.String.Regex (replace)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, throw)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import Test.PMock.Cons (Cons(..), (#>), type (#>))
import Test.PMock.Param (Param(..), cons, (:>), value, param, any, matcher, or, and, notEqual)
import Test.PMock.ParamDivider (class ParamDivider, args, returnValue)
import Test.Spec.Assertions (fail)

data Mock fun params = Mock (Maybe MockName) fun (Verifier params)
type MockName = String

mock :: forall params fun verifyParams m
   . MockBuilder params fun verifyParams
  => MonadEffect m
  => params
  -> m (Mock fun verifyParams)
mock params = liftEffect $ build Nothing params

namedMock :: forall params fun verifyParams m
   . MockBuilder params fun verifyParams
  => MonadEffect m
  => MockName
  -> params
  -> m (Mock fun verifyParams)
namedMock name params = liftEffect $ build (Just name) params

class MockBuilder params fun verifyParams | params -> fun, params -> verifyParams where
  build :: Maybe MockName -> params -> Effect (Mock fun verifyParams)

instance instanceMockArrayArg9 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h, Show i, Eq i)
  => MockBuilder
    (Array (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param i #> Param r))
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param i) where
  build name params = do
    s <- store
    createMock name s.calledParamsList (\a2 b2 c2 d2 e2 f2 g2 h2 i2 -> findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2 :> p i2) s)
else
instance instanceMockArrayArg8 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h)
  => MockBuilder
    (Array (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param r))
    (a -> b -> c -> d -> e -> f -> g -> h -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h) where
  build name params = do
    s <- store
    createMock name s.calledParamsList (\a2 b2 c2 d2 e2 f2 g2 h2 -> findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2) s)
else
instance instanceMockArrayArg7 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g)
  => MockBuilder
    (Array (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param r))
    (a -> b -> c -> d -> e -> f -> g -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g) where
  build name params = do
    s <- store
    createMock name s.calledParamsList (\a2 b2 c2 d2 e2 f2 g2 -> findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2) s)
else
instance instanceMockArrayArg6 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f)
  => MockBuilder
    (Array (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param r))
    (a -> b -> c -> d -> e -> f -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f) where
  build name params = do
    s <- store
    createMock name s.calledParamsList (\a2 b2 c2 d2 e2 f2 -> findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2) s)
else
instance instanceMockArrayArg5 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e)
  => MockBuilder
    (Array (Param a #> Param b #> Param c #> Param d #> Param e #> Param r))
    (a -> b -> c -> d -> e -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e) where
  build name params = do
    s <- store
    createMock name s.calledParamsList (\a2 b2 c2 d2 e2 -> findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2) s)
else
instance instanceMockArrayArg4 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d)
  => MockBuilder
    (Array (Param a #> Param b #> Param c #> Param d #> Param r))
    (a -> b -> c -> d -> r)
    (Param a #> Param b #> Param c #> Param d) where
  build name params = do
    s <- store
    createMock name s.calledParamsList (\a2 b2 c2 d2 -> findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2) s)
else
instance instanceMockArrayArg3 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c)
  => MockBuilder
    (Array (Param a #> Param b #> Param c #> Param r))
    (a -> b -> c -> r)
    (Param a #> Param b #> Param c) where
  build name params = do
    s <- store
    createMock name s.calledParamsList (\a2 b2 c2 -> findReturnValueWithStore name params (p a2 :> p b2 :> p c2) s)
else
instance instanceMockArrayArg2 :: (Show a, Eq a, Show b, Eq b)
  => MockBuilder (Array (Param a #> Param b #> Param r)) (a -> b -> r) (Param a #> Param b) where
  build name params = do
    s <- store
    createMock name s.calledParamsList (\a2 b2 -> findReturnValueWithStore name params (p a2 :> p b2) s)
else
instance instanceMockArrayArg1 :: (Show a, Eq a)
  => MockBuilder (Array (Param a #> Param r)) (a -> r) (Param a) where
  build name params = do
    s <- store
    createMock name s.calledParamsList (\a2 -> findReturnValueWithStore name params (p a2) s)
else
instance instanceMockArg9 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h, Show i, Eq i)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param i #> Param r)
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param i) where
  build name params = do
    s <- store
    createMock name s.calledParamsList (\a2 b2 c2 d2 e2 f2 g2 h2 i2 ->
      extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2 :> p i2) s)
else
instance instanceMockArg8 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param r)
    (a -> b -> c -> d -> e -> f -> g -> h -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h) where
  build name params = do
    s <- store
    createMock name s.calledParamsList (\a2 b2 c2 d2 e2 f2 g2 h2 ->
      extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2) s)
else
instance instanceMockArg7 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param r)
    (a -> b -> c -> d -> e -> f -> g -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g) where
  build name params = do
    s <- store
    createMock name s.calledParamsList (\a2 b2 c2 d2 e2 f2 g2 ->
      extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2) s)
else
instance instanceMockArg6 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param r)
    (a -> b -> c -> d -> e -> f -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f) where
  build name params = do
    s <- store
    createMock name s.calledParamsList (\a2 b2 c2 d2 e2 f2 -> extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2) s)
else
instance instanceMockArg5 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param r)
    (a -> b -> c -> d -> e -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e) where
  build name params = do
    s <- store
    createMock name s.calledParamsList (\a2 b2 c2 d2 e2 -> extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2) s)
else
instance instanceMockArg4 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param r)
    (a -> b -> c -> d -> r)
    (Param a #> Param b #> Param c #> Param d) where
  build name params = do
    s <- store
    createMock name s.calledParamsList (\a2 b2 c2 d2 -> extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2) s)
else
instance instanceMockArg3 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c)
  => MockBuilder (Param a #> Param b #> Param c #> Param r) (a -> b -> c -> r) (Param a #> Param b #> Param c) where
  build name params = do
    s <- store
    createMock name s.calledParamsList (\a2 b2 c2 -> extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2) s)
else
instance instanceMockArg2 :: (Show a, Eq a, Show b, Eq b)
  => MockBuilder (Param a #> Param b #> Param r) (a -> b -> r) (Param a #> Param b) where
  build name params = do
    s <- store
    createMock name s.calledParamsList (\a2 b2 -> extractReturnValueWithValidate name params (p a2 :> p b2) s)
else
instance instanceMockArg1 :: (Show a, Eq a)
  => MockBuilder (Param a #> Param r) (a -> r) (Param a) where
  build name params = do
    s <- store
    createMock name s.calledParamsList (\a2 -> extractReturnValueWithValidate name params (p a2) s)

createMock :: forall fun params. Eq params => Show params => Maybe MockName -> CalledParamsList params -> fun -> Effect (Mock fun params)
createMock name l fn = pure $ Mock name fn (Verifier l)

foreign import store :: forall params. Effect (CalledParamsStore params)

type CalledParamsStore params = {
  calledParamsList :: CalledParamsList params,
  store :: params -> Effect Unit
}

type CalledParamsList params = Array params

newtype Verifier params = Verifier (CalledParamsList params)

type Message = String

newtype VerifyFailed = VerifyFailed Message

extractReturnValueWithValidate ∷ forall params args r.
     ParamDivider params args (Param r)
  => Eq args
  => Show args
  => Maybe MockName
  -> params
  -> args
  -> CalledParamsStore args
  -> r
extractReturnValueWithValidate name params inputParams s =
  let
    expectedArgs = args params
    r = returnValue params
    _ = validateWithStoreParams name s expectedArgs inputParams
  in r

findReturnValue :: forall params args r.
     Eq args
  => ParamDivider params args (Param r)
  => CalledParamsList params
  -> args
  -> Maybe r
findReturnValue paramsList inputParams = do
  find (\params -> (args params) == inputParams) paramsList
    >>= \params -> pure $ returnValue params

findReturnValueWithStore :: forall params args r.
     Eq args
  => Show args
  => ParamDivider params args (Param r)
  => Maybe MockName
  -> CalledParamsList params
  -> args
  -> CalledParamsStore args
  -> r
findReturnValueWithStore name paramsList inputParams s =
  let
    _ = storeCalledParams s inputParams
    expectedArgs = args <$> paramsList
  in case findReturnValue paramsList inputParams of
    Just v -> v
    Nothing -> error $ messageForMultiMock name expectedArgs inputParams

fun :: forall fun v. Mock fun v -> fun
fun (Mock _ f _) = f

mockFun ::
  forall params fun verifyParams m
  . MockBuilder params fun verifyParams
  => MonadEffect m
  => params
  -> m fun
mockFun params = mock params <#> fun

namedMockFun ::
  forall params fun verifyParams m
  . MockBuilder params fun verifyParams
  => MonadEffect m
  => String
  -> params
  -> m fun
namedMockFun name params = namedMock name params <#> fun

validateWithStoreParams :: forall a. Eq a => Show a => Maybe MockName -> CalledParamsStore a -> a -> a -> Unit
validateWithStoreParams name s expected actual = validateParams name expected (storeCalledParams s actual)

validateParams :: forall a. Eq a => Show a => Maybe MockName -> a -> a -> Unit
validateParams name expected actual =
  if (expected == actual) then unit
  else error $ message name expected actual

storeCalledParams :: forall a. CalledParamsStore a -> a -> a
storeCalledParams s a = const a (s.store a)

data VerifyMatchType a = MatchAny a | MatchAll a

class Verify params input where
  verify :: forall fun m. MonadThrow Error m => Mock fun params -> input -> m Unit

instance instanceVerifyParamType :: (Eq a, Show a) => Verify (Param a) (VerifyMatchType (Param a)) where
  verify v a = _verify v a
else
instance instanceVerifyParam :: (Eq a, Show a) => Verify (Param a) a where
  verify v a = _verify v (MatchAny (param a))
else
instance instanceVerifyType :: (Eq a, Show a) => Verify a (VerifyMatchType a) where
  verify v a = _verify v a
else
instance instanceVerify :: (Eq a, Show a) => Verify a a where
  verify v a = _verify v (MatchAny a)

hasBeenCalledWith
  :: forall @params @input fun m
   . Verify params input
  => MonadThrow Error m
  => Mock fun params
  -> input
  -> m Unit
hasBeenCalledWith = verify

_verify :: forall fun params m. Eq params => Show params => MonadThrow Error m => Mock fun params -> VerifyMatchType params -> m Unit
_verify (Mock name _ (Verifier calledParamsList)) matcher =
  case doVerify name calledParamsList matcher of
    Just (VerifyFailed msg) -> fail msg
    Nothing -> pure unit

doVerify :: forall a. Eq a => Show a => Maybe MockName -> CalledParamsList a -> VerifyMatchType a -> Maybe VerifyFailed
doVerify name list (MatchAny a) = do
  guard $ A.all (a /= _) list
  pure $ verifyFailedMesssage name list a
doVerify name list (MatchAll a) = do
  guard $ A.any (a /= _) list
  pure $ verifyFailedMesssage name list a

verifyFailedMesssage :: forall a. Show a => Maybe MockName -> CalledParamsList a -> a -> VerifyFailed
verifyFailedMesssage name calledParams expected =
  VerifyFailed $ joinWith "\n"
    ["function" <> mockNameLabel name <> "was not called with expected arguments.",
     "  expected: " <> show expected,
     "  but was : " <> formatCalledParamsList calledParams]

formatCalledParamsList :: forall a. Show a => CalledParamsList a -> String
formatCalledParamsList calledParams
  | length calledParams == 0 =
    "Never been called."
  | length calledParams == 1 =
    show calledParams # (replace (unsafeRegex "^\\[" noFlags) "") >>> (replace (unsafeRegex "]$" noFlags) "")
  | otherwise = show calledParams

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
  verifyCount :: forall fun m. MonadThrow Error m => Eq params => Mock fun params -> countType -> a -> m Unit

instance instanceVerifyCount3 ::  Eq a => VerifyCount CountVerifyMethod (Param a) a where
  verifyCount v count a = _verifyCount v (param a) count
else
instance instanceVerifyCountParam1 :: Eq a => VerifyCount Int (Param a) a where
  verifyCount v count a =  _verifyCount v (param a) (Equal count)
else
instance instanceVerifyCount2 :: VerifyCount CountVerifyMethod a a where
  verifyCount v count a = _verifyCount v a count
else
instance instanceVerifyCount :: VerifyCount Int a a where
  verifyCount v count a = _verifyCount v a (Equal count)

_verifyCount :: forall fun params m. MonadThrow Error m => Eq params => Mock fun params -> params -> CountVerifyMethod -> m Unit
_verifyCount (Mock name _ (Verifier calledParamsList)) v method =
  let
    callCount = length (filter (\args -> v == args) calledParamsList)
  in if compareCount method callCount then pure unit
    else fail $ joinWith "\n" [
      "function" <> mockNameLabel name <> "was not called the expected number of times.",
      "  expected: " <> show method,
      "  but was : " <> show callCount]

hasBeenCalledTimes
  :: forall @countType @params @a fun m
   . VerifyCount countType params a
  => MonadThrow Error m
  => Eq params
  => Mock fun params
  -> countType
  -> a
  -> m Unit
hasBeenCalledTimes = verifyCount

hasBeenCalledTimesGreaterThanEqual
  :: forall params a fun m
   . VerifyCount CountVerifyMethod params a
  => MonadThrow Error m
  => Eq params
  => Mock fun params
  -> Int
  -> a
  -> m Unit
hasBeenCalledTimesGreaterThanEqual m i = hasBeenCalledTimes m (GreaterThanEqual i)

hasBeenCalledTimesLessThanEqual
  :: forall params a fun m
   . VerifyCount CountVerifyMethod params a
  => MonadThrow Error m
  => Eq params
  => Mock fun params
  -> Int
  -> a
  -> m Unit
hasBeenCalledTimesLessThanEqual m i = hasBeenCalledTimes m (LessThanEqual i)

hasBeenCalledTimesGreaterThan
  :: forall params a fun m
   . VerifyCount CountVerifyMethod params a
  => MonadThrow Error m
  => Eq params
  => Mock fun params
  -> Int
  -> a
  -> m Unit
hasBeenCalledTimesGreaterThan m i = hasBeenCalledTimes m (GreaterThan i)

hasBeenCalledTimesLessThan
  :: forall params a fun m
   . VerifyCount CountVerifyMethod params a
  => MonadThrow Error m
  => Eq params
  => Mock fun params
  -> Int
  -> a
  -> m Unit
hasBeenCalledTimesLessThan m i = hasBeenCalledTimes m (LessThan i)

with :: forall a m. MonadThrow Error m => (a -> m Unit) -> a -> m Unit
with f a = f a 

hasNotBeenCalledWith
  :: forall params a fun m
   . VerifyCount Int params a
  => MonadThrow Error m
  => Eq params
  => Mock fun params
  -> a
  -> m Unit
hasNotBeenCalledWith v a = verifyCount v 0 a

showCalledParams :: forall fun params. Show params => Mock fun params -> String
showCalledParams (Mock _ _ (Verifier calledParamsList)) = show calledParamsList

{-
  Function was not called with expected arguments.
  expected: 1, "2", 3
  but was : 1, "1", 1
-}
message :: forall a. Show a => Maybe MockName -> a -> a -> String
message name expected actual =
  joinWith "\n" [
    "function" <> mockNameLabel name <> "was not called with expected arguments.",
    "  expected: " <> show expected,
    "  but was : " <> show actual
  ]

{-
  Function was not called with expected arguments.
  expected one of the following:
    "a", 100
    "b", 200
  but was actual: "a", 200
-}
messageForMultiMock :: forall a. Show a => Maybe MockName -> Array a -> a -> String
messageForMultiMock name expecteds actual =
  joinWith "\n" [
    "function" <> mockNameLabel name <> "was not called with expected arguments.",
    "  expected one of the following:",
    joinWith "\n" $ ("    " <> _) <<< show <$> expecteds,
    "  but was actual:",
    ("    " <> _) <<< show $ actual
  ]

mockNameLabel :: Maybe MockName -> String
mockNameLabel = fromMaybe " " <<< enclose " " <<< enclose "`"

enclose :: String -> Maybe String -> Maybe String
enclose e = map (\v -> e <> v <> e)

error :: forall a. String -> a
error = unsafePerformEffect <<< throw

p :: forall a. a -> Param a
p = param

class VerifyOrder params input where
  verifySequence :: forall fun m. MonadThrow Error m => Mock fun params -> Array input -> m Unit
  verifyPartiallySequence :: forall fun m. MonadThrow Error m => Mock fun params -> Array input -> m Unit

instance instanceVerifyParamOrder :: (Eq a, Show a) => VerifyOrder (Param a) a where
  verifySequence v a = _verifyOrder ExactlySequence v $ param <$> a
  verifyPartiallySequence v a = _verifyOrder PartiallySequence v $ param <$> a
else
instance instanceVerifyOrder :: (Eq a, Show a) => VerifyOrder a a where
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
   . Eq params
  => Show params
  => MonadThrow Error m
  => VerifyOrderMethod
  -> Mock fun params
  -> Array params
  -> m Unit
_verifyOrder method (Mock name _ (Verifier calledParamsList)) matchers =
  case doVerifyOrder method name calledParamsList matchers of
    Just (VerifyFailed msg) -> fail msg
    Nothing -> pure unit

data VerifyOrderMethod
  = ExactlySequence
  | PartiallySequence

doVerifyOrder :: forall a. Eq a => Show a => VerifyOrderMethod -> Maybe MockName -> CalledParamsList a -> Array a -> Maybe VerifyFailed
doVerifyOrder ExactlySequence name calledValues expectedValues
  | length calledValues /= length expectedValues = do
    let header = "The number of function" <> (mockNameLabel name) <> "calls doesn't match the number of params."
    pure $ verifyFailedOrderParamCountMismatch header calledValues expectedValues
  | otherwise = do
    let unexpectedOrders = collectUnExpectedOrder calledValues expectedValues
    guard $ length unexpectedOrders > 0
    pure $ verifyFailedSequence name unexpectedOrders

doVerifyOrder PartiallySequence name calledValues expectedValues
  | length calledValues < length expectedValues = do
    let header = "The number of parameters exceeds the number of function"  <> (mockNameLabel name) <> "calls."
    pure $ verifyFailedOrderParamCountMismatch header calledValues expectedValues
  | otherwise = do
    guard $ isOrderNotMatched calledValues expectedValues
    pure $ verifyFailedPartiallySequence name calledValues expectedValues

type VerifyOrderResult a = {
  index :: Int,
  calledValue :: a,
  expectedValue :: a
}

collectUnExpectedOrder :: forall a. Eq a => Show a => CalledParamsList a -> Array a -> Array (VerifyOrderResult a)
collectUnExpectedOrder calledValues expectedValues =
  catMaybes $ mapWithIndex (\i expectedValue -> do
    let calledValue = unsafeIndex calledValues i
    guard $ expectedValue /= calledValue
    pure {index: i, calledValue, expectedValue}
  ) expectedValues

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex arr idx =
  case arr !! idx of
    Just a -> a
    Nothing -> unsafeCrashWith "Array is too short"

verifyFailedOrderParamCountMismatch :: forall a. Show a => String -> CalledParamsList a -> Array a -> VerifyFailed
verifyFailedOrderParamCountMismatch header calledValues expectedValues =
  VerifyFailed $ joinWith "\n"
    [header,
    "  number of function calls: " <> (show $ length calledValues),
    "  number of params:         " <> (show $ length expectedValues)]

verifyFailedPartiallySequence :: forall a. Show a => Maybe MockName -> CalledParamsList a -> Array a -> VerifyFailed
verifyFailedPartiallySequence name calledValues expectedValues =
  VerifyFailed $ joinWith "\n"
    ["function" <> mockNameLabel name <> "was not called with expected order.",
     "  expected order:",
     joinWith "\n" $ ("    " <> _) <<< show <$> expectedValues,
     "  actual order:",
     joinWith "\n" $ ("    " <> _) <<< show <$> calledValues]

isOrderNotMatched :: forall a. Eq a => CalledParamsList a -> Array a -> Boolean
isOrderNotMatched calledValues expectedValues =
  isNothing $ foldl (\candidates e -> do
    candidates >>= \c -> do
      index <- A.elemIndex e c
      Just $ A.drop (index + 1) c
  )
  (Just calledValues) expectedValues

verifyFailedSequence :: forall a. Show a => Maybe MockName -> Array (VerifyOrderResult a) -> VerifyFailed
verifyFailedSequence name fails =
  VerifyFailed $ joinWith "\n" $ A.cons ("function" <> mockNameLabel name <> "was not called with expected order.") $ verifyOrderFailedMesssage <$> fails

verifyOrderFailedMesssage :: forall a. Show a => VerifyOrderResult a -> String
verifyOrderFailedMesssage {index, calledValue, expectedValue} =
  let callCount = showHumanReadable (index + 1)
  in joinWith "\n"
    ["  expected " <> callCount <> " call: " <> show expectedValue,
     "  but was  " <> callCount <> " call: " <> show calledValue]
  where
  showHumanReadable :: Int -> String
  showHumanReadable 1 = "1st"
  showHumanReadable 2 = "2nd"
  showHumanReadable 3 = "3rd"
  showHumanReadable n = show n <> "th"
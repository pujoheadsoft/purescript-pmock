module Test.PMock
  (
  module Test.PMock.Cons,
  module Test.PMock.Param,
  class MockBuilder,
  mock,
  verify,
  class VerifyCount,
  verifyCount,
  class Verify,
  Verifier,
  showCalledParams,
  Mock,
  CountVerifyMethod(..),
  fun,
  mockFun
  )
  where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (filter, find, length)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String.Regex (replace)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect.Exception (Error, throw)
import Effect.Unsafe (unsafePerformEffect)
import Test.PMock.Cons (Cons(..), (#>), type (#>))
import Test.PMock.Param (Param(..), cons, (:>), value, param, any, matcher)
import Test.PMock.ParamDivider (class ParamDivider, args, returnValue)
import Test.Spec.Assertions (fail)

data Mock fun params = Mock fun (Verifier params)

class MockBuilder params fun verifyParams | params -> fun, params -> verifyParams where
  mock :: params -> Mock fun verifyParams

instance instanceMockArrayArg9 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h, Show i, Eq i)
  => MockBuilder 
    (Array (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param i #> Param r))
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param i) where
  mock params = do
    let s = store unit
    createMock s.calledParamsList (\a2 b2 c2 d2 e2 f2 g2 h2 i2 -> findReturnValueWithStore params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2 :> p i2) s)
else
instance instanceMockArrayArg8 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h)
  => MockBuilder 
    (Array (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param r))
    (a -> b -> c -> d -> e -> f -> g -> h -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h) where
  mock params = do
    let s = store unit
    createMock s.calledParamsList (\a2 b2 c2 d2 e2 f2 g2 h2 -> findReturnValueWithStore params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2) s)
else
instance instanceMockArrayArg7 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g)
  => MockBuilder 
    (Array (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param r))
    (a -> b -> c -> d -> e -> f -> g -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g) where
  mock params = do
    let s = store unit
    createMock s.calledParamsList (\a2 b2 c2 d2 e2 f2 g2 -> findReturnValueWithStore params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2) s)
else
instance instanceMockArrayArg6 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f)
  => MockBuilder 
    (Array (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param r))
    (a -> b -> c -> d -> e -> f -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f) where
  mock params = do
    let s = store unit
    createMock s.calledParamsList (\a2 b2 c2 d2 e2 f2 -> findReturnValueWithStore params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2) s)
else
instance instanceMockArrayArg5 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e)
  => MockBuilder 
    (Array (Param a #> Param b #> Param c #> Param d #> Param e #> Param r))
    (a -> b -> c -> d -> e -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e) where
  mock params = do
    let s = store unit
    createMock s.calledParamsList (\a2 b2 c2 d2 e2 -> findReturnValueWithStore params (p a2 :> p b2 :> p c2 :> p d2 :> p e2) s)
else
instance instanceMockArrayArg4 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d)
  => MockBuilder 
    (Array (Param a #> Param b #> Param c #> Param d #> Param r))
    (a -> b -> c -> d -> r)
    (Param a #> Param b #> Param c #> Param d) where
  mock params = do
    let s = store unit
    createMock s.calledParamsList (\a2 b2 c2 d2 -> findReturnValueWithStore params (p a2 :> p b2 :> p c2 :> p d2) s)
else
instance instanceMockArrayArg3 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c)
  => MockBuilder 
    (Array (Param a #> Param b #> Param c #> Param r))
    (a -> b -> c -> r)
    (Param a #> Param b #> Param c) where
  mock params = do
    let s = store unit
    createMock s.calledParamsList (\a2 b2 c2 -> findReturnValueWithStore params (p a2 :> p b2 :> p c2) s)
else
instance instanceMockArrayArg2 :: (Show a, Eq a, Show b, Eq b)
  => MockBuilder (Array (Param a #> Param b #> Param r)) (a -> b -> r) (Param a #> Param b) where
  mock params = do
    let s = store unit
    createMock s.calledParamsList (\a2 b2 -> findReturnValueWithStore params (p a2 :> p b2) s)
else
instance instanceMockArrayArg1 :: (Show a, Eq a)
  => MockBuilder (Array (Param a #> Param r)) (a -> r) (Param a) where
  mock params = do
    let s = store unit
    createMock s.calledParamsList (\a2 -> findReturnValueWithStore params (p a2) s)
else
instance instanceMockArg9 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h, Show i, Eq i)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param i #> Param r)
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param i) where
  mock params = do
    let s = store unit
    createMock s.calledParamsList (\a2 b2 c2 d2 e2 f2 g2 h2 i2 -> 
      extractReturnValueWithValidate params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2 :> p i2) s)
else
instance instanceMockArg8 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param r)
    (a -> b -> c -> d -> e -> f -> g -> h -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h) where
  mock params = do
    let s = store unit
    createMock s.calledParamsList (\a2 b2 c2 d2 e2 f2 g2 h2 -> 
      extractReturnValueWithValidate params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2) s)
else
instance instanceMockArg7 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param r)
    (a -> b -> c -> d -> e -> f -> g -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g) where
  mock params = do
    let s = store unit
    createMock s.calledParamsList (\a2 b2 c2 d2 e2 f2 g2 -> 
      extractReturnValueWithValidate params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2) s)
else
instance instanceMockArg6 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param r)
    (a -> b -> c -> d -> e -> f -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f) where
  mock params = do
    let s = store unit
    createMock s.calledParamsList (\a2 b2 c2 d2 e2 f2 -> extractReturnValueWithValidate params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2) s)
else
instance instanceMockArg5 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param r)
    (a -> b -> c -> d -> e -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e) where
  mock params = do
    let s = store unit
    createMock s.calledParamsList (\a2 b2 c2 d2 e2 -> extractReturnValueWithValidate params (p a2 :> p b2 :> p c2 :> p d2 :> p e2) s)
else
instance instanceMockArg4 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param r)
    (a -> b -> c -> d -> r)
    (Param a #> Param b #> Param c #> Param d) where
  mock params = do
    let s = store unit
    createMock s.calledParamsList (\a2 b2 c2 d2 -> extractReturnValueWithValidate params (p a2 :> p b2 :> p c2 :> p d2) s)
else
instance instanceMockArg3 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c)
  => MockBuilder (Param a #> Param b #> Param c #> Param r) (a -> b -> c -> r) (Param a #> Param b #> Param c) where
  mock params = do
    let s = store unit
    createMock s.calledParamsList (\a2 b2 c2 -> extractReturnValueWithValidate params (p a2 :> p b2 :> p c2) s)
else
instance instanceMockArg2 :: (Show a, Eq a, Show b, Eq b)
  => MockBuilder (Param a #> Param b #> Param r) (a -> b -> r) (Param a #> Param b) where
  mock params = do
    let s = store unit
    createMock s.calledParamsList (\a2 b2 -> extractReturnValueWithValidate params (p a2 :> p b2) s)
else
instance instanceMockArg1 :: (Show a, Eq a) 
  => MockBuilder (Param a #> Param r) (a -> r) (Param a) where
  mock params = do
    let s = store unit
    createMock s.calledParamsList (\a2 -> extractReturnValueWithValidate params (p a2) s)

createMock :: forall fun params. Eq params => Show params => CalledParamsList params -> fun -> Mock fun params
createMock l fn = Mock fn (Verifier l)

foreign import store :: forall params. Unit -> CalledParamsStore params

type CalledParamsStore params = {
  calledParamsList :: CalledParamsList params,
  store :: params -> Unit
}

type CalledParamsList params = Array params

newtype Verifier params = Verifier (CalledParamsList params)                                 -- called parameters list

type Message = String

newtype VerifyFailed = VerifyFailed Message

extractReturnValueWithValidate ∷ forall params args r. 
     ParamDivider params args (Param r)
  => Eq args 
  => Show args 
  => params 
  -> args
  -> CalledParamsStore args
  -> r
extractReturnValueWithValidate params inputParams s =
  let
    expectedArgs = args params
    r = returnValue params
    _ = validateWithStoreParams s expectedArgs inputParams
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
  => ParamDivider params args (Param r)
  => CalledParamsList params
  -> args
  -> CalledParamsStore args
  -> r
findReturnValueWithStore paramsList inputParams s =
  let
    _ = storeCalledParams s inputParams
  in case findReturnValue paramsList inputParams of
    Just v -> v
    Nothing -> error "no answer found."

fun :: forall fun v. Mock fun v -> fun
fun (Mock f _) = f

mockFun ::
  forall params fun verifyParams
  . MockBuilder params fun verifyParams
  => params
  -> fun
mockFun params = mock params # fun

validateWithStoreParams :: forall a. Eq a => Show a => CalledParamsStore a -> a -> a -> Unit
validateWithStoreParams s expected actual = validateParams expected (storeCalledParams s actual)

validateParams :: forall a. Eq a => Show a => a -> a -> Unit
validateParams expected actual = if (expected == actual) then unit else error $ message expected actual

storeCalledParams :: forall a. CalledParamsStore a -> a -> a
storeCalledParams s a = const a (s.store a)

class Verify params input where
  verify :: forall fun m. MonadThrow Error m => Mock fun params -> input -> m Unit

instance instanceVerifyParam :: (Eq a, Show a) => Verify (Param a) a where
  verify v a = _verify v (param a)
else
instance instanceVerify :: (Eq a, Show a) => Verify a a where
  verify v a = _verify v a

_verify :: forall fun params m. Eq params => Show params => MonadThrow Error m => Mock fun params -> params -> m Unit
_verify (Mock _ (Verifier calledParamsList)) inputParams =
  case doVerify calledParamsList inputParams of
    Just (VerifyFailed msg) -> fail msg
    Nothing -> pure unit

doVerify :: forall a. Eq a => Show a => CalledParamsList a -> a -> Maybe VerifyFailed
doVerify list a = 
  if A.any (a == _) list then Nothing 
  else Just $ verifyFailedMesssage list a

verifyFailedMesssage :: forall a. Show a => CalledParamsList a -> a -> VerifyFailed
verifyFailedMesssage calledParams expected = 
  VerifyFailed $ joinWith "\n" 
    ["Function was not called with expected arguments.",  
     "  expected: " <> show expected, 
     "  but was : " <> formatCalledParamsList calledParams]


formatCalledParamsList :: forall a. Show a => CalledParamsList a -> String
formatCalledParamsList calledParams = do
  if length calledParams == 1 then
    show calledParams # (replace (unsafeRegex "^\\[" noFlags) "") >>> (replace (unsafeRegex "]$" noFlags) "")
  else show calledParams

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
_verifyCount (Mock _ (Verifier calledParamsList)) v method = 
  let
    callCount = length (filter (\args -> v == args) calledParamsList)
  in if compareCount method callCount then pure unit
    else fail $ joinWith "\n" ["Function was not called the expected number of times.",  "  expected: " <> show method, "  but was : " <> show callCount]

showCalledParams :: forall fun params. Show params => Mock fun params -> String
showCalledParams (Mock _ (Verifier calledParamsList)) = show calledParamsList

{-
  Function was not called with expected arguments.
  expected: 1, "2", 3
  but was : 1, "1", 1
-}
message :: forall a. Show a => a -> a -> String
message expected actual = joinWith "\n" ["Function was not called with expected arguments.",  "  expected: " <> show expected, "  but was : " <> show actual]

error :: forall a. String -> a
error = unsafePerformEffect <<< throw

p :: forall a. a -> Param a
p = param
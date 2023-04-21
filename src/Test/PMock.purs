module Test.PMock
  (
  module Test.PMock.Cons,
  module Test.PMock.Param,
  class MockBuilder,
  mock,
  verify,
  class VerifyCountBuilder,
  verifyCount,
  class VerifyBuilder,
  Verifier,
  showCalledParams,
  Mock,
  runRuntimeThrowableFunction,
  CountVerifyMethod(..)
  )
  where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (filter, find, length)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Effect.Exception (Error, throw)
import Effect.Unsafe (unsafePerformEffect)
import Test.PMock.Cons (Cons(..), (#>), type (#>))
import Test.PMock.Param (Param(..), cons, (:>), value, param, any, matcher)
import Test.PMock.ParamDivider (class ParamDivider, args, returnValue)
import Test.Spec.Assertions (fail)

type Mock fn v = {
  fun :: fn,
  verifier :: Verifier v
}

foreign import store :: forall a. Unit -> CallredParamsStore a

type CallredParamsStore v = {
  argsList :: CalledParamsList v,
  store :: v -> Unit
}

type CalledParamsList v = Array v

newtype Verifier v = Verifier {
  calledParamsList :: CalledParamsList v,
  verifyFun :: CalledParamsList v -> v -> Maybe VerifyFailed
}

verifier :: forall v. CalledParamsList v -> (CalledParamsList v -> v -> Maybe VerifyFailed) -> Verifier v
verifier l f = Verifier { calledParamsList: l, verifyFun: f }

type Message = String

data VerifyFailed = VerifyFailed Message

class MockBuilder a r v | a -> r, a -> v where
  mock :: a -> Mock r v

instance instanceMockArrayArg9 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h, Show i, Eq i)
  => MockBuilder 
    (Array (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param i #> Param r))
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param i) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 c2 d2 e2 f2 g2 h2 i2 -> findReturnValueWithStore defs (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2 :> p i2) s)
else
instance instanceMockArrayArg8 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h)
  => MockBuilder 
    (Array (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param r))
    (a -> b -> c -> d -> e -> f -> g -> h -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 c2 d2 e2 f2 g2 h2 -> findReturnValueWithStore defs (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2) s)
else
instance instanceMockArrayArg7 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g)
  => MockBuilder 
    (Array (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param r))
    (a -> b -> c -> d -> e -> f -> g -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 c2 d2 e2 f2 g2 -> findReturnValueWithStore defs (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2) s)
else
instance instanceMockArrayArg6 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f)
  => MockBuilder 
    (Array (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param r))
    (a -> b -> c -> d -> e -> f -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 c2 d2 e2 f2 -> findReturnValueWithStore defs (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2) s)
else
instance instanceMockArrayArg5 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e)
  => MockBuilder 
    (Array (Param a #> Param b #> Param c #> Param d #> Param e #> Param r))
    (a -> b -> c -> d -> e -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 c2 d2 e2 -> findReturnValueWithStore defs (p a2 :> p b2 :> p c2 :> p d2 :> p e2) s)
else
instance instanceMockArrayArg4 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d)
  => MockBuilder 
    (Array (Param a #> Param b #> Param c #> Param d #> Param r))
    (a -> b -> c -> d -> r)
    (Param a #> Param b #> Param c #> Param d) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 c2 d2 -> findReturnValueWithStore defs (p a2 :> p b2 :> p c2 :> p d2) s)
else
instance instanceMockArrayArg3 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c)
  => MockBuilder 
    (Array (Param a #> Param b #> Param c #> Param r))
    (a -> b -> c -> r)
    (Param a #> Param b #> Param c) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 c2 -> findReturnValueWithStore defs (p a2 :> p b2 :> p c2) s)
else
instance instanceMockArrayArg2 :: (Show a, Eq a, Show b, Eq b)
  => MockBuilder (Array (Param a #> Param b #> Param r)) (a -> b -> r) (Param a #> Param b) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 -> findReturnValueWithStore defs (p a2 :> p b2) s)
else
instance instanceMockArrayArg1 :: (Show a, Eq a)
  => MockBuilder (Array (Param a #> Param r)) (a -> r) (Param a) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 -> findReturnValueWithStore defs (p a2) s)
else
instance instanceMockArg9 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h, Show i, Eq i)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param i #> Param r)
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param i) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 c2 d2 e2 f2 g2 h2 i2 -> 
      extractReturnValueWithValidate defs (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2 :> p i2) s)
else
instance instanceMockArg8 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param r)
    (a -> b -> c -> d -> e -> f -> g -> h -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 c2 d2 e2 f2 g2 h2 -> 
      extractReturnValueWithValidate defs (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2) s)
else
instance instanceMockArg7 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param r)
    (a -> b -> c -> d -> e -> f -> g -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 c2 d2 e2 f2 g2 -> 
      extractReturnValueWithValidate defs (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2) s)
else
instance instanceMockArg6 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param r)
    (a -> b -> c -> d -> e -> f -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 c2 d2 e2 f2 -> extractReturnValueWithValidate defs (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2) s)
else
instance instanceMockArg5 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param r)
    (a -> b -> c -> d -> e -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 c2 d2 e2 -> extractReturnValueWithValidate defs (p a2 :> p b2 :> p c2 :> p d2 :> p e2) s)
else
instance instanceMockArg4 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param r)
    (a -> b -> c -> d -> r)
    (Param a #> Param b #> Param c #> Param d) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 c2 d2 -> extractReturnValueWithValidate defs (p a2 :> p b2 :> p c2 :> p d2) s)
else
instance instanceMockArg3 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c)
  => MockBuilder (Param a #> Param b #> Param c #> Param r) (a -> b -> c -> r) (Param a #> Param b #> Param c) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 c2 -> extractReturnValueWithValidate defs (p a2 :> p b2 :> p c2) s)
else
instance instanceMockArg2 :: (Show a, Eq a, Show b, Eq b)
  => MockBuilder (Param a #> Param b #> Param r) (a -> b -> r) (Param a #> Param b) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 -> extractReturnValueWithValidate defs (p a2 :> p b2) s)
else
instance instanceMockArg1 :: (Show a, Eq a) 
  => MockBuilder (Param a #> Param r) (a -> r) (Param a) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 -> extractReturnValueWithValidate defs (p a2) s)

extractReturnValueWithValidate ∷ forall d a r. 
     ParamDivider d a (Param r)
  => Eq a 
  => Show a 
  => d 
  -> a
  -> CallredParamsStore a → r
extractReturnValueWithValidate defs params s =
  let
    a = args defs
    r = returnValue defs
    _ = validateWithStoreParams s a params
  in r

findReturnValue :: forall d a r. 
     Eq a 
  => ParamDivider d a (Param r)
  => Array d
  -> a
  -> Maybe r
findReturnValue defsList inputArgs = do
  find (\defs -> (args defs) == inputArgs) defsList
    >>= \defs -> pure $ returnValue defs

findReturnValueWithStore :: forall d a r.
     Eq a
  => ParamDivider d a (Param r)
  => CalledParamsList d
  -> a
  -> CallredParamsStore a
  -> r
findReturnValueWithStore defsList inputArgs s =
  let
    _ = storeCalledParams s inputArgs
  in case findReturnValue defsList inputArgs of
    Just v -> v
    Nothing -> error "no answer found."

mockT :: forall fun v. Eq v => Show v => CalledParamsList v -> fun -> Mock fun v
mockT argsList fun = {
  fun,
  verifier: verifier argsList (\list args -> doVerify list args)
}

p :: forall a. a -> Param a
p = param

doVerify :: forall a. Eq a => Show a => Array a -> a -> Maybe VerifyFailed
doVerify list a = 
  if A.any (a == _) list then Nothing 
  else Just $ verifyFailedMesssage list a

verifyFailedMesssage :: forall a. Show a => Array a -> a -> VerifyFailed
verifyFailedMesssage calledParams expected
  = VerifyFailed $ joinWith "\n" ["Function was not called with expected arguments.",  "  expected: " <> show expected, "  but was : " <> show calledParams]

class VerifyBuilder v a where
  verify :: forall m r. MonadThrow Error m => { verifier :: Verifier v | r} -> a -> m Unit

instance instanceVerifyBuilderParam1 :: Eq a => VerifyBuilder (Param a) a where
  verify v a = _verify v (param a)
else
instance instanceVerifyBuilder :: VerifyBuilder a a where
  verify v args = _verify v args

_verify :: forall v m r. MonadThrow Error m => { verifier :: Verifier v | r} -> v -> m Unit
_verify {verifier : (Verifier { calledParamsList, verifyFun })} args =
  case verifyFun calledParamsList args of
    Just (VerifyFailed msg) -> fail msg
    Nothing -> pure unit

validateParams :: forall a. Eq a => Show a => a -> a -> Unit
validateParams expected actual = if (expected == actual) then unit else error $ message expected actual

storeCalledParams :: forall a. CallredParamsStore a -> a -> a
storeCalledParams s a = const a (s.store a)

validateWithStoreParams :: forall a. Eq a => Show a => CallredParamsStore a -> a -> a -> Unit
validateWithStoreParams s expected actual = validateParams expected (storeCalledParams s actual)

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

class VerifyCountBuilder c v a where
  verifyCount :: forall m r. MonadThrow Error m => Eq v => { verifier :: Verifier v | r} -> c -> a -> m Unit

instance instanceVerifyCountBuilder3 ::  Eq a => VerifyCountBuilder CountVerifyMethod (Param a) a where
  verifyCount v count a = _verifyCount v (param a) count
else
instance instanceVerifyCountBuilderParam1 :: Eq a => VerifyCountBuilder Int (Param a) a where
  verifyCount v count a =  _verifyCount v (param a) (Equal count)
else
instance instanceVerifyCountBuilder2 :: VerifyCountBuilder CountVerifyMethod a a where
  verifyCount v count a = _verifyCount v a count
else
instance instanceVerifyCountBuilder :: VerifyCountBuilder Int a a where
  verifyCount v count a = _verifyCount v a (Equal count)

_verifyCount :: forall v m r. MonadThrow Error m => Eq v => { verifier :: Verifier v | r} -> v -> CountVerifyMethod -> m Unit
_verifyCount {verifier : (Verifier { calledParamsList })} v method = 
  let
    callCount = length (filter (\args -> v == args) calledParamsList)
  in if compareCount method callCount then pure unit
    else fail $ joinWith "\n" ["Function was not called the expected number of times.",  "expected: " <> show method, "but was : " <> show callCount]

showCalledParams :: forall v r. Eq v => Show v => { verifier :: Verifier v | r} -> v -> String
showCalledParams { verifier : (Verifier { calledParamsList }) } v = show (filter (\args -> args == v) calledParamsList)

{-
  Function was not called with expected arguments.
  expected: 1, "2", 3
  but was : 1, "1", 1
-}
message :: forall a. Show a => a -> a -> String
message expected actual = joinWith "\n" ["Function was not called with expected arguments.",  "  expected: " <> show expected, "  but was : " <> show actual]

error :: forall a. String -> a
error = unsafePerformEffect <<< throw

type TryCatchResult r = {
  hasError :: Boolean,
  error :: String,
  result :: r
}

foreign import _runRuntimeThrowableFunction :: forall r. (Unit -> r) -> TryCatchResult r

runRuntimeThrowableFunction :: forall r m. MonadThrow Error m => (Unit -> r) -> m Unit
runRuntimeThrowableFunction f =
  let
    r = _runRuntimeThrowableFunction f
  in if r.hasError then fail r.error else pure unit

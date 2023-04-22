module Test.PMockSpecs 
  ( it
  , mockIt
  , runRuntimeThrowableFunction
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Effect.Exception (Error)
import Test.Spec (SpecT)
import Test.Spec as Spec
import Test.Spec.Assertions (fail)

foreign import _runRuntimeThrowableFunction :: forall r. (Unit -> r) -> TryCatchResult r

type TryCatchResult r = {
  hasError :: Boolean,
  error :: String,
  result :: r
}

it :: forall m g. Monad m => MonadError Error g => String -> (Unit -> g Unit) -> SpecT g Unit m Unit
it name fun = 
  Spec.it name do
    let
      r = _runRuntimeThrowableFunction fun
    if r.hasError then fail r.error else r.result

mockIt :: forall m g. Monad m => MonadError Error g => String -> (Unit -> g Unit) -> SpecT g Unit m Unit
mockIt = it
    
runRuntimeThrowableFunction :: forall r m. MonadThrow Error m => (Unit -> r) -> m Unit
runRuntimeThrowableFunction f =
  let
    r = _runRuntimeThrowableFunction f
  in if r.hasError then fail r.error else pure unit

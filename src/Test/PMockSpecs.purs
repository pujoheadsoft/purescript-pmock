module Test.PMockSpecs
  ( it
  , mockIt
  , runRuntimeThrowableFunction
  , expectErrorWithMessage
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, try)
import Data.Either (Either(..))
import Data.String (joinWith)
import Effect.Exception (Error, message)
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

expectErrorWithMessage
  :: forall m t
   . MonadError Error m
  => String
  -> m t
  -> m Unit
expectErrorWithMessage msg a =
  try a >>= case _ of
    Left e ->
      if message e == msg then pure unit
      else fail $ joinWith "\n" [
        "Error message is different from expected error message",
        "expected error message:",
        msg,
        "",
        "but was:",
        message e
      ]
    Right _ -> fail "expected error"
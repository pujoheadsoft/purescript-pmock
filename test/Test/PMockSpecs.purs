module Test.PMockSpecs
  ( module PMockSpec
  , runRuntimeThrowableFunction
  , expectErrorWithMessage
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, try)
import Data.Either (Either(..))
import Data.String (joinWith)
import Effect.Exception (Error, message)
import Test.PMock.Spec (mockIt) as PMockSpec
import Test.Spec.Assertions (fail)

foreign import _runRuntimeThrowableFunction :: forall r. (Unit -> r) -> TryCatchResult r

type TryCatchResult r =
  { hasError :: Boolean
  , error :: String
  , result :: r
  }

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

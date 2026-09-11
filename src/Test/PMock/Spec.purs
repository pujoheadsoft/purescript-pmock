module Test.PMock.Spec
  ( mockIt
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Effect.Exception (Error)
import Test.Spec (SpecT)
import Test.Spec as Spec
import Test.Spec.Assertions (fail)

foreign import runRuntimeThrowableFunction :: forall r. (Unit -> r) -> TryCatchResult r

type TryCatchResult r =
  { hasError :: Boolean
  , error :: String
  , result :: r
  }

mockIt
  :: forall m g
   . Monad m
  => MonadError Error g
  => String
  -> (Unit -> g Unit)
  -> SpecT g Unit m Unit
mockIt name test =
  Spec.it name do
    let result = runRuntimeThrowableFunction test
    if result.hasError then fail result.error else result.result

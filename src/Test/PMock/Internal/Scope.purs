module Test.PMock.Internal.Scope
  ( expects
  , registerExpectations
  , class ShouldBeCalled
  , shouldBeCalled
  , verifyCalls
  , withMock
  , withMocks
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error, try, throw, throwException)
import Test.PMock.Internal.Builder (MockCreation(..))
import Test.PMock.Internal.Expectation (Expectations, TimesSpec, Verification, called, calledWith, verifyExpectations, verifyVerification)
import Test.PMock.Internal.Message (verificationFailureMessage)
import Test.PMock.Internal.FunctionParams (class FunctionParams)
import Test.PMock.Internal.Param (class MatchParams)
import Test.PMock.Internal.Registry (lookupRecorder, registerRecorder)
import Test.PMock.Internal.Types (Mock(..))
import Test.PMock.Internal.Verify (class ToDirectExpected)

foreign import data Scope :: Type

foreign import openScope :: Effect Scope

foreign import closeScope :: Scope -> Effect (Array (Effect Unit))

foreign import currentScopeImpl
  :: (Scope -> Maybe Scope)
  -> Maybe Scope
  -> Effect (Maybe Scope)

foreign import addVerification :: Scope -> Effect Unit -> Effect Unit

foreign import deferVerification
  :: (Unit -> Effect Unit)
  -> Effect Unit

currentScope :: Effect (Maybe Scope)
currentScope = currentScopeImpl Just Nothing

withMocks :: forall a. Effect a -> Effect a
withMocks action = do
  scope <- openScope
  actionResult <- try action
  verifications <- closeScope scope
  verificationResult <- try $ traverse_ identity verifications
  case actionResult, verificationResult of
    Right result, Right _ -> pure result
    Right _, Left verificationError -> throwException verificationError
    Left actionError, _ -> throwException actionError

withMock :: forall a. Effect a -> Effect a
withMock = withMocks

expects
  :: forall fun params
   . MockCreation fun params
  -> Expectations params Unit
  -> Effect fun
expects (MockCreation create) expectations = do
  built@(Mock _ fn _) <- create
  registerRecorder fn built
  registerExpectations built expectations
  pure fn

registerExpectations
  :: forall fun params
   . Mock fun params
  -> Expectations params Unit
  -> Effect Unit
registerExpectations built expectations = do
  scope <- currentScope >>= case _ of
    Just activeScope -> pure activeScope
    Nothing -> throw "expects must be used inside withMock."
  addVerification scope $ deferVerification \_ ->
    verifyExpectations built expectations

verifyCalls
  :: forall fun params m
   . FunctionParams fun params
  => MonadEffect m
  => MonadThrow Error m
  => fun
  -> Expectations params Unit
  -> m Unit
verifyCalls fn expectations = do
  registered <- liftEffect $ lookupRecorder fn
  case registered of
    Just (built :: Mock fun params) -> verifyExpectations built expectations
    Nothing -> throwError $ error verificationFailureMessage

class ShouldBeCalled fun specification where
  shouldBeCalled
    :: forall m
     . MonadEffect m
    => MonadThrow Error m
    => fun
    -> specification
    -> m Unit

instance shouldBeCalledVerification ::
  FunctionParams fun params =>
  ShouldBeCalled fun (Verification params) where
  shouldBeCalled fn verification =
    resolveMock fn \built -> verifyVerification built verification
else instance shouldBeCalledCount ::
  FunctionParams fun params =>
  ShouldBeCalled fun TimesSpec where
  shouldBeCalled fn method =
    resolveMock fn \built -> verifyExpectations built (called method)
else instance shouldBeCalledArgument ::
  ( FunctionParams fun params
  , ToDirectExpected input params
  , MatchParams params
  ) => ShouldBeCalled fun input where
  shouldBeCalled fn input =
    resolveMock fn \built -> verifyVerification built (calledWith input)

resolveMock
  :: forall fun params m
   . FunctionParams fun params
  => MonadEffect m
  => MonadThrow Error m
  => fun
  -> (Mock fun params -> m Unit)
  -> m Unit
resolveMock fn verifyRegistered = do
  registered <- liftEffect $ lookupRecorder fn
  case registered of
    Just (built :: Mock fun params) -> verifyRegistered built
    Nothing -> throwError $ error verificationFailureMessage

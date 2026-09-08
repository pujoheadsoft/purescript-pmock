module Test.PMock.Builder
  ( class MockBuilder
  , class MockSequenceBuilder
  , fun
  , mock
  , mockSequence
  , namedMockSequence
  , namedMock
  , build
  , buildSequence
  , mockFun
  , namedMockFun
  ) where

import Prelude

import Data.Array (filter, find, length, (!!))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Test.PMock.Cons (type (#>))
import Test.PMock.CurryArgs (class CurryArgs, curryArgs)
import Test.PMock.Message (message, messageForMultiMock, mockNameLabel)
import Test.PMock.Param (Param, param)
import Test.PMock.ParamDivider (class ParamDivider, args, returnValue)
import Test.PMock.Types (CalledParamsList, Mock(..), MockName, Verifier(..))

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

mockSequence :: forall params fun verifyParams m
   . MockSequenceBuilder params fun verifyParams
  => MonadEffect m
  => params
  -> m (Mock fun verifyParams)
mockSequence params = liftEffect $ buildSequence Nothing params

namedMockSequence :: forall params fun verifyParams m
   . MockSequenceBuilder params fun verifyParams
  => MonadEffect m
  => MockName
  -> params
  -> m (Mock fun verifyParams)
namedMockSequence name params = liftEffect $ buildSequence (Just name) params

class MockBuilder params fun verifyParams | params -> fun, params -> verifyParams where
  build :: Maybe MockName -> params -> Effect (Mock fun verifyParams)

class MockSequenceBuilder params fun verifyParams | params -> fun, params -> verifyParams where
  buildSequence :: Maybe MockName -> params -> Effect (Mock fun verifyParams)

instance instanceMockEffect :: MockBuilder (Effect r) (Effect r) (Param Unit) where
  build name returned = do
    s <- store
    createMock name s.calledParamsList do
      s.store (p unit)
      returned

instance instanceMockSequenceEffect :: MockSequenceBuilder (Array (Effect r)) (Effect r) (Param Unit) where
  buildSequence name returns = do
    s <- store
    createMock name s.calledParamsList do
      s.store (p unit)
      let callIndex = length s.calledParamsList - 1
      case returns !! min callIndex (length returns - 1) of
        Just returned -> returned
        Nothing -> throw $ "function" <> mockNameLabel name <> "has no sequential return values."

else
instance instanceMockSequenceArgs ::
  ( ParamDivider (Param a #> tail) args (Param r)
  , CurryArgs args r fun
  , Eq args
  , Show args
  ) =>
  MockSequenceBuilder (Array (Param a #> tail)) fun args where
  buildSequence name params = do
    s <- store
    createMock name s.calledParamsList
      (curryArgs (\inputParams -> findSequentialReturnValueWithStore name params inputParams s))

instance instanceMockArrayArgs ::
  ( ParamDivider (Param a #> tail) args (Param r)
  , CurryArgs args r fun
  , Eq args
  , Show args
  ) =>
  MockBuilder (Array (Param a #> tail)) fun args where
  build name params = do
    s <- store
    createMock name s.calledParamsList
      (curryArgs (\inputParams -> findReturnValueWithStore name params inputParams s))

instance instanceMockArgs ::
  ( ParamDivider (Param a #> tail) args (Param r)
  , CurryArgs args r fun
  , Eq args
  , Show args
  ) =>
  MockBuilder (Param a #> tail) fun args where
  build name params = do
    s <- store
    createMock name s.calledParamsList
      (curryArgs (\inputParams -> extractReturnValueWithValidate name params inputParams s))

createMock :: forall fun params. Eq params => Show params => Maybe MockName -> CalledParamsList params -> fun -> Effect (Mock fun params)
createMock name l fn = pure $ Mock name fn (Verifier l)

foreign import store :: forall params. Effect (CalledParamsStore params)

type CalledParamsStore params = {
  calledParamsList :: CalledParamsList params,
  store :: params -> Effect Unit
}

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

findSequentialReturnValueWithStore :: forall params args r.
     Eq args
  => Show args
  => ParamDivider params args (Param r)
  => Maybe MockName
  -> CalledParamsList params
  -> args
  -> CalledParamsStore args
  -> r
findSequentialReturnValueWithStore name paramsList inputParams s =
  let
    callIndex = length $ filter (_ == inputParams) s.calledParamsList
    _ = storeCalledParams s inputParams
    matchingParams = filter (\params -> args params == inputParams) paramsList
    selected = matchingParams !! min callIndex (length matchingParams - 1)
    expectedArgs = args <$> paramsList
  in case selected of
    Just params -> returnValue params
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
storeCalledParams s a = unsafePerformEffect do
  s.store a
  pure a

error :: forall a. String -> a
error = unsafePerformEffect <<< throw

p :: forall a. a -> Param a
p = param

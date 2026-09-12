module Test.PMock.Internal.Builder
  ( class MockBuilder
  , class MockSequenceBuilder
  , class MockFn
  , MockCreation(..)
  , fun
  , label
  , mock
  , mockHandle
  , mockFunction
  , mockSequence
  , namedMockSequence
  , namedMock
  , build
  , buildSequence
  , mockFun
  , namedMockFun
  ) where

import Prelude

import Data.Array (filter, find, findIndex, length, mapMaybe, (!!))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Test.PMock.Internal.Cons (type (#>))
import Test.PMock.Internal.CurryArgs (class CurryArgs, curryArgs)
import Test.PMock.Internal.Message (messageForMultiMockFromRendered, messageFromRendered, mockNameLabel)
import Test.PMock.Internal.Param
  ( class MatchParams
  , Param
  , matchesParams
  , param
  , renderActualParams
  , renderExpectedParams
  )
import Test.PMock.Internal.ParamDivider (class ParamDivider, class ReturnValue, args, returnValue)
import Test.PMock.Internal.Registry (registerRecorder)
import Test.PMock.Internal.Stub (Cases, caseValues)
import Test.PMock.Internal.Types (CalledParamsList, Label(..), Mock(..), MockName, Verifier(..))

newtype MockCreation fun params = MockCreation (Effect (Mock fun params))

label :: MockName -> Label
label = Label

mockHandle :: forall params fun verifyParams m
   . MockBuilder params fun verifyParams
  => MonadEffect m
  => params
  -> m (Mock fun verifyParams)
mockHandle params = liftEffect $ build Nothing params

class MockFn input output where
  mock :: input -> output

instance mockFnLabeledCreation ::
  MockBuilder params fun verifyParams =>
  MockFn Label (params -> MockCreation fun verifyParams) where
  mock (Label name) = buildCreation (Just name)
else instance mockFnLabeled ::
  ( MockBuilder params fun verifyParams
  , MonadEffect m
  ) => MockFn Label (params -> m fun) where
  mock (Label name) = buildRegistered (Just name)
else instance mockFnCreation ::
  MockBuilder params fun verifyParams =>
  MockFn params (MockCreation fun verifyParams) where
  mock = buildCreation Nothing
else instance mockFnUnlabeled ::
  ( MockBuilder params fun verifyParams
  , MonadEffect m
  ) => MockFn params (m fun) where
  mock = buildRegistered Nothing

buildCreation :: forall params fun verifyParams
   . MockBuilder params fun verifyParams
  => Maybe MockName
  -> params
  -> MockCreation fun verifyParams
buildCreation name params = MockCreation $ build name params

buildRegistered :: forall params fun verifyParams m
   . MockBuilder params fun verifyParams
  => MonadEffect m
  => Maybe MockName
  -> params
  -> m fun
buildRegistered name params = liftEffect do
  built@(Mock _ fn _) <- build name params
  registerRecorder fn built
  pure fn

mockFunction :: forall params fun verifyParams m
   . MockBuilder params fun verifyParams
  => MonadEffect m
  => params
  -> m fun
mockFunction = buildRegistered Nothing

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
  ( ParamDivider (Param a #> tail) args result
  , ReturnValue result r
  , CurryArgs args r fun
  , MatchParams args
  ) =>
  MockSequenceBuilder (Array (Param a #> tail)) fun args where
  buildSequence name params = do
    s <- store
    createMock name s.calledParamsList
      (curryArgs (\inputParams -> findSequentialReturnValueWithStore name params inputParams s))

instance instanceMockArrayArgs ::
  ( ParamDivider (Param a #> tail) args result
  , ReturnValue result r
  , CurryArgs args r fun
  , MatchParams args
  ) =>
  MockBuilder (Array (Param a #> tail)) fun args where
  build name params = do
    s <- store
    createMock name s.calledParamsList
      (curryArgs (\inputParams -> findReturnValueWithStore name params inputParams s))

instance instanceMockCasesEffect ::
  MockBuilder (Cases (Effect r) Unit) (Effect r) (Param Unit) where
  build name definitions = do
    s <- store
    createMock name s.calledParamsList do
      s.store (p unit)
      let callIndex = length s.calledParamsList - 1
      case caseValues definitions !! 0 of
        Just responses -> case responses !! min callIndex (length responses - 1) of
          Just returned -> returned
          Nothing -> throw $ "function" <> mockNameLabel name <> "has no return values."
        Nothing -> throw $ "function" <> mockNameLabel name <> "has no return values."

else instance instanceMockCases ::
  ( ParamDivider (Param a #> tail) args result
  , ReturnValue result r
  , CurryArgs args r fun
  , MatchParams args
  ) => MockBuilder (Cases (Param a #> tail) Unit) fun args where
  build name definitions = do
    s <- store
    createMock name s.calledParamsList
      (curryArgs (\inputParams -> findCaseReturnValueWithStore name
        (caseValues definitions) inputParams s))

instance instanceMockArgs ::
  ( ParamDivider (Param a #> tail) args result
  , ReturnValue result r
  , CurryArgs args r fun
  , MatchParams args
  ) =>
  MockBuilder (Param a #> tail) fun args where
  build name params = do
    s <- store
    createMock name s.calledParamsList
      (curryArgs (\inputParams -> extractReturnValueWithValidate name params inputParams s))

createMock :: forall fun params. Maybe MockName -> CalledParamsList params -> fun -> Effect (Mock fun params)
createMock name l fn = pure $ Mock name fn (Verifier l)

foreign import store :: forall params. Effect (CalledParamsStore params)

type CalledParamsStore params = {
  calledParamsList :: CalledParamsList params,
  store :: params -> Effect Unit
}

extractReturnValueWithValidate ∷ forall params args result r.
     ParamDivider params args result
  => ReturnValue result r
  => MatchParams args
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

findReturnValue :: forall params args result r.
     MatchParams args
  => ParamDivider params args result
  => ReturnValue result r
  => CalledParamsList params
  -> args
  -> Maybe r
findReturnValue paramsList inputParams = do
  find (\params -> matchesParams (args params) inputParams) paramsList
    >>= \params -> pure $ returnValue params

findReturnValueWithStore :: forall params args result r.
     MatchParams args
  => ParamDivider params args result
  => ReturnValue result r
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
    Nothing -> error $ messageForMultiMockFromRendered name
      (renderExpectedParams <$> expectedArgs)
      (renderActualForExpectedList expectedArgs inputParams)

findSequentialReturnValueWithStore :: forall params args result r.
     MatchParams args
  => ParamDivider params args result
  => ReturnValue result r
  => Maybe MockName
  -> CalledParamsList params
  -> args
  -> CalledParamsStore args
  -> r
findSequentialReturnValueWithStore name paramsList inputParams s =
  let
    matchingParams = filter (\params -> matchesParams (args params) inputParams) paramsList
    matchingCasesFor actual =
      (\params -> matchesParams (args params) actual) <$> paramsList
    matchingCases = matchingCasesFor inputParams
    callIndex = length $ filter
      (\called -> matchingCasesFor called == matchingCases)
      s.calledParamsList
    _ = storeCalledParams s inputParams
    selected = matchingParams !! min callIndex (length matchingParams - 1)
    expectedArgs = args <$> paramsList
  in case selected of
    Just params -> returnValue params
    Nothing -> error $ messageForMultiMockFromRendered name
      (renderExpectedParams <$> expectedArgs)
      (renderActualForExpectedList expectedArgs inputParams)

findCaseReturnValueWithStore :: forall params args result r.
     MatchParams args
  => ParamDivider params args result
  => ReturnValue result r
  => Maybe MockName
  -> Array (Array params)
  -> args
  -> CalledParamsStore args
  -> r
findCaseReturnValueWithStore name caseList inputParams s =
  let
    selectedCaseIndex actual = findIndex
      (\responses -> case responses !! 0 of
        Just params -> matchesParams (args params) actual
        Nothing -> false)
      caseList
    selectedIndex = selectedCaseIndex inputParams
    callIndex = case selectedIndex of
      Just index -> length $ filter (\called -> selectedCaseIndex called == Just index)
        s.calledParamsList
      Nothing -> 0
    _ = storeCalledParams s inputParams
    selected = selectedIndex >>= \index -> caseList !! index
      >>= \responses -> responses !! min callIndex (length responses - 1)
    expectedArgs = mapMaybe (_ !! 0) caseList <#> args
  in case selected of
    Just params -> returnValue params
    Nothing -> error $ messageForMultiMockFromRendered name
      (renderExpectedParams <$> expectedArgs)
      (renderActualForExpectedList expectedArgs inputParams)

fun :: forall fun v. Mock fun v -> fun
fun (Mock _ f _) = f

mockFun ::
  forall params fun verifyParams m
  . MockBuilder params fun verifyParams
  => MonadEffect m
  => params
  -> m fun
mockFun params = mockHandle params <#> fun

namedMockFun ::
  forall params fun verifyParams m
  . MockBuilder params fun verifyParams
  => MonadEffect m
  => String
  -> params
  -> m fun
namedMockFun name params = namedMock name params <#> fun

validateWithStoreParams :: forall a. MatchParams a => Maybe MockName -> CalledParamsStore a -> a -> a -> Unit
validateWithStoreParams name s expected actual = validateParams name expected (storeCalledParams s actual)

validateParams :: forall a. MatchParams a => Maybe MockName -> a -> a -> Unit
validateParams name expected actual =
  if matchesParams expected actual then unit
  else error $ messageFromRendered name
    (renderExpectedParams expected)
    (renderActualParams expected actual)

storeCalledParams :: forall a. CalledParamsStore a -> a -> a
storeCalledParams s a = unsafePerformEffect do
  s.store a
  pure a

error :: forall a. String -> a
error = unsafePerformEffect <<< throw

p :: forall a. Eq a => Show a => a -> Param a
p = param

renderActualForExpectedList :: forall a. MatchParams a => Array a -> a -> String
renderActualForExpectedList expecteds actual = case expecteds !! 0 of
  Just expected -> renderActualParams expected actual
  Nothing -> "<actual value>"

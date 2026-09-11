module Test.PMock.Internal.Stub
  ( class StubBuilder
  , class StubFn
  , buildStub
  , stub
  , Cases
  , onCase
  , cases
  , caseValues
  ) where

import Prelude

import Data.Array (find, (!!))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Test.PMock.Internal.Cons (type (#>))
import Test.PMock.Internal.CurryArgs (class CurryArgs, curryArgs)
import Test.PMock.Internal.Message (messageForMultiMockFromRendered, messageFromRendered)
import Test.PMock.Internal.Param
  ( class MatchParams
  , Param
  , matchesParams
  , renderActualParams
  , renderExpectedParams
  )
import Test.PMock.Internal.ParamDivider
  ( class ParamDivider
  , class ReturnValue
  , args
  , returnValue
  )
import Test.PMock.Internal.Types (Label(..), MockName)

class StubBuilder params fun | params -> fun where
  buildStub :: Maybe MockName -> params -> fun

newtype Cases a b = Cases
  { values :: Array a
  , result :: b
  }

instance functorCases :: Functor (Cases a) where
  map f (Cases definition) = Cases
    { values: definition.values
    , result: f definition.result
    }

instance applyCases :: Apply (Cases a) where
  apply (Cases functions) (Cases values) = Cases
    { values: functions.values <> values.values
    , result: functions.result values.result
    }

instance applicativeCases :: Applicative (Cases a) where
  pure result = Cases { values: [], result }

instance bindCases :: Bind (Cases a) where
  bind (Cases definition) next =
    let Cases nextDefinition = next definition.result
    in Cases
      { values: definition.values <> nextDefinition.values
      , result: nextDefinition.result
      }

instance monadCases :: Monad (Cases a)

onCase :: forall a. a -> Cases a Unit
onCase value = Cases { values: [ value ], result: unit }

cases :: forall a. Array a -> Cases a Unit
cases values = Cases { values, result: unit }

caseValues :: forall a b. Cases a b -> Array a
caseValues (Cases definition) = definition.values

class StubFn input output where
  stub :: input -> output

instance stubFnLabeled ::
  StubBuilder params fun => StubFn Label (params -> fun) where
  stub (Label name) = buildStub (Just name)
else instance stubFnUnlabeled ::
  StubBuilder params fun => StubFn params fun where
  stub = buildStub Nothing

instance stubBuilderEffect :: StubBuilder (Effect r) (Effect r) where
  buildStub _ = identity

else
instance stubBuilderCases ::
  ( ParamDivider (Param a #> tail) args result
  , ReturnValue result r
  , CurryArgs args r fun
  , MatchParams args
  ) => StubBuilder (Cases (Param a #> tail) Unit) fun where
  buildStub name (Cases definition) = curryArgs \inputParams ->
    let paramsList = definition.values
    in
    case find (\params -> matchesParams (args params) inputParams) paramsList of
      Just params -> returnValue params
      Nothing ->
        let expectedArgs = args <$> paramsList
        in error $ messageForMultiMockFromRendered name
          (renderExpectedParams <$> expectedArgs)
          (renderActualForExpectedList expectedArgs inputParams)

else
instance stubBuilderArgs ::
  ( ParamDivider (Param a #> tail) args result
  , ReturnValue result r
  , CurryArgs args r fun
  , MatchParams args
  ) => StubBuilder (Param a #> tail) fun where
  buildStub name params = curryArgs \inputParams ->
    let expectedArgs = args params
    in if matchesParams expectedArgs inputParams then
      returnValue params
    else
      error $ messageFromRendered name
        (renderExpectedParams expectedArgs)
        (renderActualParams expectedArgs inputParams)

error :: forall a. String -> a
error = unsafePerformEffect <<< throw

renderActualForExpectedList :: forall a. MatchParams a => Array a -> a -> String
renderActualForExpectedList expecteds actual = case expecteds !! 0 of
  Just expected -> renderActualParams expected actual
  Nothing -> "<actual value>"

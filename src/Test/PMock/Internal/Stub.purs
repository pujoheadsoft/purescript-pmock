module Test.PMock.Internal.Stub
  ( class StubBuilder
  , class StubFn
  , buildStub
  , stub
  , Cases
  , Case
  , class AndThen
  , andThen
  , class OnCase
  , class ReplaceReturn
  , replaceReturn
  , onCase
  , cases
  , caseValues
  ) where

import Prelude

import Data.Array (find, mapMaybe, (!!))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Test.PMock.Internal.Cons (type (#>), (#>))
import Test.PMock.Internal.CurryArgs (class CurryArgs, curryArgs)
import Test.PMock.Internal.Message (messageForMultiMockFromRendered, messageFromRendered)
import Test.PMock.Internal.Param
  ( class MatchParams
  , Param
  , matchesParams
  , param
  , renderActualParams
  , renderExpectedParams
  , Result(..)
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

newtype Case a = Case (Array a)

newtype Cases a b = Cases
  { values :: Array (Case a)
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

class OnCase input a | input -> a where
  onCase :: input -> Cases a Unit

instance onCaseCase :: OnCase (Case a) a where
  onCase value = Cases { values: [ value ], result: unit }
else instance onCaseValue :: OnCase a a where
  onCase value = Cases { values: [ Case [ value ] ], result: unit }

class ReplaceReturn params r | params -> r where
  replaceReturn :: params -> r -> params

instance replaceReturnParam :: (Eq r, Show r) =>
  ReplaceReturn (Param a #> Param r) r where
  replaceReturn (a #> _) r = a #> param r
else instance replaceReturnResult ::
  ReplaceReturn (Param a #> Result r) r where
  replaceReturn (a #> _) r = a #> Result r
else instance replaceReturnMore ::
  ReplaceReturn (Param b #> rest) r =>
  ReplaceReturn (Param a #> Param b #> rest) r where
  replaceReturn (a #> rest) r = a #> replaceReturn rest r

class AndThen input response output | input -> response output where
  andThen :: input -> response -> output

instance andThenEffectCase ::
  AndThen (Case (Effect r)) (Effect r) (Case (Effect r)) where
  andThen (Case responses) response = Case (responses <> [ response ])
else instance andThenCase :: ReplaceReturn params r =>
  AndThen (Case params) r (Case params) where
  andThen (Case responses) response = case responses !! 0 of
    Just first -> Case (responses <> [ replaceReturn first response ])
    Nothing -> Case []
else instance andThenEffect ::
  AndThen (Effect r) (Effect r) (Case (Effect r)) where
  andThen first response = Case [ first, response ]
else instance andThenParams :: ReplaceReturn params r =>
  AndThen params r (Case params) where
  andThen params response = Case [ params, replaceReturn params response ]

cases :: forall a. Array a -> Cases a Unit
cases values = Cases { values: map (Case <<< pure) values, result: unit }

caseValues :: forall a b. Cases a b -> Array (Array a)
caseValues (Cases definition) = caseResponses <$> definition.values

caseResponses :: forall a. Case a -> Array a
caseResponses (Case responses) = responses

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
    let paramsList = mapMaybe (_ !! 0) (caseResponses <$> definition.values)
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

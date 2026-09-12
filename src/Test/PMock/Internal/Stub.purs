module Test.PMock.Internal.Stub
  ( class StubBuilder
  , class StubFn
  , buildStub
  , stub
  , Cases
  , Responses
  , class AndThen
  , andThen
  , class OnCase
  , class NormalizeCase
  , normalizeCase
  , class ReturnResponses
  , returnResponses
  , returnResponsesOf
  , responseValues
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
import Test.PMock.Internal.Cons (type (#>), (#>))
import Test.PMock.Internal.CurryArgs (class CurryArgs, curryArgs)
import Test.PMock.Internal.Message (messageForMultiMockFromRendered, messageFromRendered)
import Test.PMock.Internal.Param
  ( class MatchParams
  , Param
  , Result(..)
  , matchesParams
  , renderActualParams
  , renderExpectedParams
  , value
  )
import Test.PMock.Internal.ParamDivider
  ( class ParamDivider
  , class ReturnValue
  , args
  , return
  , returnValue
  , returnValueOf
  )
import Test.PMock.Internal.Types (Label(..), MockName)

class StubBuilder params fun | params -> fun where
  buildStub :: Maybe MockName -> params -> fun

newtype Responses a = Responses (Array a)

responseValues :: forall a. Responses a -> Array a
responseValues (Responses responses) = responses

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

class AndThen input response output | input -> response output where
  andThen :: input -> response -> output

instance andThenResponses ::
  AndThen (Responses r) r (Responses r) where
  andThen (Responses responses) response = Responses (responses <> [ response ])
else instance andThenValue ::
  AndThen r r (Responses r) where
  andThen first response = Responses [ first, response ]

class NormalizeCase params normalized | params -> normalized where
  normalizeCase :: params -> normalized

instance normalizeCaseResultResponses ::
  NormalizeCase
    (Param a #> Result (Responses r))
    (Param a #> Result (Responses r)) where
  normalizeCase = identity
else instance normalizeCaseResult ::
  NormalizeCase
    (Param a #> Result r)
    (Param a #> Result (Responses r)) where
  normalizeCase (a #> Result response) = a #> Result (Responses [ response ])
else instance normalizeCaseParam ::
  NormalizeCase
    (Param a #> Param r)
    (Param a #> Result (Responses r)) where
  normalizeCase (a #> response) = a #> Result (Responses [ value response ])
else instance normalizeCaseMore ::
  NormalizeCase (Param b #> rest) normalized =>
  NormalizeCase
    (Param a #> Param b #> rest)
    (Param a #> normalized) where
  normalizeCase (a #> rest) = a #> normalizeCase rest

class OnCase input a | input -> a where
  onCase :: input -> Cases a Unit

instance onCaseResponses :: OnCase (Responses r) (Responses r) where
  onCase responses = Cases { values: [ responses ], result: unit }
else instance onCaseEffect :: OnCase (Effect r) (Responses (Effect r)) where
  onCase response = Cases
    { values: [ Responses [ response ] ]
    , result: unit
    }
else instance onCaseParams :: NormalizeCase params normalized =>
  OnCase params normalized where
  onCase params = Cases { values: [ normalizeCase params ], result: unit }

class ReturnResponses result r | result -> r where
  returnResponsesOf :: result -> Array r

instance returnResponsesResultSequence ::
  ReturnResponses (Result (Responses r)) r where
  returnResponsesOf result = case returnValueOf result of
    Responses responses -> responses
else instance returnResponsesParamSequence ::
  ReturnResponses (Param (Responses r)) r where
  returnResponsesOf result = case returnValueOf result of
    Responses responses -> responses
else instance returnResponsesResult ::
  ReturnResponses (Result r) r where
  returnResponsesOf result = [ returnValueOf result ]
else instance returnResponsesParam ::
  ReturnResponses (Param r) r where
  returnResponsesOf result = [ returnValueOf result ]

returnResponses :: forall params args result r.
  ParamDivider params args result => ReturnResponses result r => params -> Array r
returnResponses = return >>> returnResponsesOf

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
  , ReturnResponses result r
  , CurryArgs args r fun
  , MatchParams args
  ) => StubBuilder (Cases (Param a #> tail) Unit) fun where
  buildStub name (Cases definition) = curryArgs \inputParams ->
    let paramsList = definition.values
    in
    case find (\params -> matchesParams (args params) inputParams) paramsList of
      Just params -> case returnResponses params !! 0 of
        Just response -> response
        Nothing -> error $ "function has no return values."
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

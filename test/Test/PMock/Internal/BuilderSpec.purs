module Test.PMock.Internal.BuilderSpec (builderSpec) where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Either (isLeft)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Test.PMock.Internal.Builder (fun, mock, mockFunction, mockHandle, mockSequence, namedMockSequence)
import Test.PMock.Internal.Message (verificationFailureMessage)
import Test.PMock.Internal.Param (Param, any, matcher, matcherBy, (:>))
import Test.PMockSpecs (expectErrorWithMessage, runRuntimeThrowableFunction)
import Test.PMock.Stub (onCase)
import Test.PMock.Internal.Verify (hasBeenCalledWith, hasBeenRunTimes, hasFunctionBeenCalledTimes, hasFunctionBeenCalledWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

data Opaque = Opaque Int

builderSpec :: Spec Unit
builderSpec = describe "Builder" do
  describe "Direct function mocks" do
    it "returns a function directly from mock" do
      find <- mock $ "Aja" :> 1977

      find "Aja" `shouldEqual` 1977

    it "returns a function without requiring fun" do
      find <- mockFunction $ "Aja" :> 1977

      find "Aja" `shouldEqual` 1977

    it "defines multiple cases with a DSL" do
      find <- mockFunction do
        onCase $ "Aja" :> 1977
        onCase $ "Gaucho" :> 1980

      find "Aja" `shouldEqual` 1977
      find "Gaucho" `shouldEqual` 1980

    it "looks up its recorder for post-hoc verification" do
      let aja = matcher (_ == "Aja") "Aja"
      find <- mockFunction $ aja :> 1977
      let _ = find "Aja"

      find `hasFunctionBeenCalledWith` aja

    it "accepts a raw value for post-hoc verification" do
      find <- mockFunction $ "Aja" :> 1977
      let _ = find "Aja"

      find `hasFunctionBeenCalledWith` "Aja"

    it "rejects an ordinary function during post-hoc verification" do
      let aja = matcher (_ == "Aja") "Aja"

      expectErrorWithMessage
        verificationFailureMessage
        ((\title -> if title == "Aja" then 1977 else 0)
          `hasFunctionBeenCalledWith` aja)

    it "rejects a wrapped mock because its function identity changed" do
      let aja = matcher (_ == "Aja") "Aja"
      find <- mockFunction $ aja :> 1977
      let wrapped title = find title

      expectErrorWithMessage
        verificationFailureMessage
        (wrapped `hasFunctionBeenCalledWith` aja)

    it "records a direct Effect only when it is executed" do
      load <- mockFunction (pure 42 :: Effect Int)

      hasFunctionBeenCalledTimes load 0 unit
      result <- liftEffect load
      result `shouldEqual` 42
      hasFunctionBeenCalledTimes load 1 unit

    it "records a failing direct Effect execution" do
      failing <- mockFunction (throw "boom" :: Effect Unit)

      result <- try $ liftEffect failing

      result `shouldSatisfy` isLeft
      hasFunctionBeenCalledTimes failing 1 unit

  describe "Predicate matchers" do
    it "do not require Eq or Show for behavior or verification" do
      let positive = matcher (\(Opaque n) -> n > 0) "positive opaque" :: Param Opaque
      m <- mockHandle $ positive :> "matched"

      fun m (Opaque 1) `shouldEqual` "matched"
      m `hasBeenCalledWith` positive

    it "uses a safe placeholder when the actual value has no renderer" do
      let positive = matcher (\(Opaque n) -> n > 0) "positive opaque" :: Param Opaque
      m <- mockHandle $ positive :> "matched"

      expectErrorWithMessage
        (joinWith "\n"
          [ "Error: function was not called with the expected arguments."
          , "  expected: positive opaque"
          , "   but got: <actual value>"
          ])
        (runRuntimeThrowableFunction \_ -> fun m (Opaque 0))

    it "uses the actual renderer supplied by the matcher" do
      let
        positive = matcherBy
          { matches: \(Opaque n) -> n > 0
          , renderExpected: "positive opaque"
          , renderActual: Just \(Opaque n) -> "Opaque(" <> show n <> ")"
          }
      m <- mockHandle $ positive :> "matched"

      expectErrorWithMessage
        (joinWith "\n"
          [ "Error: function was not called with the expected arguments."
          , "  expected: positive opaque"
          , "   but got: Opaque(0)"
          , "            ^^^^^^^^^^^^^^^"
          ])
        (runRuntimeThrowableFunction \_ -> fun m (Opaque 0))

    it "does not require Eq or Show for a return value" do
      m <- mockHandle $ "input" :> Opaque 42

      case fun m "input" of
        Opaque n -> n `shouldEqual` 42

    it "uses a matcher renderer when verifying a recorded value" do
      let
        positive = matcherBy
          { matches: \(Opaque n) -> n > 0
          , renderExpected: "positive opaque"
          , renderActual: Just \(Opaque n) -> "Opaque(" <> show n <> ")"
          }
      m <- mockHandle $ any @Opaque :> "matched"
      let _ = fun m (Opaque 0)

      expectErrorWithMessage
        (joinWith "\n"
          [ "function was not called with the expected arguments."
          , "  expected: positive opaque"
          , "   but got: Opaque(0)"
          , "            ^^^^^^^^^^^^^^^"
          ])
        (m `hasBeenCalledWith` positive)

  describe "Recursive argument building" do
    it "builds and verifies a ten-argument mock" do
      m <- mockHandle $ 1 :> "2" :> true :> 4.0 :> [5] :> {six: 6} :> 7 :> "8" :> false :> 10.0 :> "result"

      fun m 1 "2" true 4.0 [5] {six: 6} 7 "8" false 10.0
        `shouldEqual` "result"
      m `hasBeenCalledWith`
        (1 :> "2" :> true :> 4.0 :> [5] :> {six: 6} :> 7 :> "8" :> false :> 10.0)

    it "builds a ten-argument multiple-response mock" do
      m <- mockHandle
        [ 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> 9 :> 10 :> "first"
        , 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> 9 :> 11 :> "second"
        ]

      fun m 1 2 3 4 5 6 7 8 9 10 `shouldEqual` "first"
      fun m 1 2 3 4 5 6 7 8 9 11 `shouldEqual` "second"

    it "builds a ten-argument sequential-response mock" do
      m <- mockSequence
        [ 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> 9 :> 10 :> "first"
        , 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> 9 :> 10 :> "second"
        ]

      fun m 1 2 3 4 5 6 7 8 9 10 `shouldEqual` "first"
      fun m 1 2 3 4 5 6 7 8 9 10 `shouldEqual` "second"

  describe "Effect actions" do
    it "does not record an action until the Effect is executed" do
      m <- mockHandle (pure 42 :: Effect Int)
      let action = fun m

      m `hasBeenRunTimes` 0

      value <- liftEffect action
      value `shouldEqual` 42
      m `hasBeenRunTimes` 1

    it "records every execution of the same Effect value" do
      m <- mockHandle (pure "result" :: Effect String)
      let action = fun m

      _ <- liftEffect action
      _ <- liftEffect action

      m `hasBeenRunTimes` 2

    it "records an execution even when the Effect fails" do
      m <- mockHandle (throw "boom" :: Effect Unit)

      result <- try $ liftEffect $ fun m

      result `shouldSatisfy` isLeft
      m `hasBeenRunTimes` 1

  describe "Sequential responses" do
    it "returns successive values for the same argument and then repeats the last" do
      m <- mockSequence
        [ "same" :> 1
        , "same" :> 2
        ]

      fun m "same" `shouldEqual` 1
      fun m "same" `shouldEqual` 2
      fun m "same" `shouldEqual` 2

    it "does not change the existing Multi Mock behavior for duplicate arguments" do
      m <- mockHandle
        [ "same" :> 1
        , "same" :> 2
        ]

      fun m "same" `shouldEqual` 1
      fun m "same" `shouldEqual` 1

    it "tracks response positions independently for different arguments" do
      m <- mockSequence
        [ "a" :> 1
        , "b" :> 10
        , "a" :> 2
        , "b" :> 20
        ]

      fun m "a" `shouldEqual` 1
      fun m "b" `shouldEqual` 10
      fun m "a" `shouldEqual` 2
      fun m "b" `shouldEqual` 20

    it "supports functions with multiple arguments" do
      m <- mockSequence
        [ "same" :> true :> 1
        , "same" :> true :> 2
        ]

      fun m "same" true `shouldEqual` 1
      fun m "same" true `shouldEqual` 2

    it "supports named sequential mocks" do
      m <- namedMockSequence "sequence"
        [ "same" :> 1
        , "same" :> 2
        ]

      fun m "same" `shouldEqual` 1
      fun m "same" `shouldEqual` 2

    it "supports successive values for an argument-free Effect" do
      m <- mockSequence
        [ pure "first" :: Effect String
        , pure "second" :: Effect String
        ]
      let action = fun m

      first <- liftEffect action
      second <- liftEffect action
      last <- liftEffect action

      first `shouldEqual` "first"
      second `shouldEqual` "second"
      last `shouldEqual` "second"
      m `hasBeenRunTimes` 3

module Test.PMock.BuilderSpec (builderSpec) where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Either (isLeft)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Test.PMock.Builder (fun, mock, mockSequence, namedMockSequence)
import Test.PMock.Param ((:>))
import Test.PMock.Verify (hasBeenCalledWith, hasBeenRunTimes)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

builderSpec :: Spec Unit
builderSpec = describe "Builder" do
  describe "Recursive argument building" do
    it "builds and verifies a ten-argument mock" do
      m <- mock $ 1 :> "2" :> true :> 4.0 :> [5] :> {six: 6} :> 7 :> "8" :> false :> 10.0 :> "result"

      fun m 1 "2" true 4.0 [5] {six: 6} 7 "8" false 10.0
        `shouldEqual` "result"
      m `hasBeenCalledWith`
        (1 :> "2" :> true :> 4.0 :> [5] :> {six: 6} :> 7 :> "8" :> false :> 10.0)

    it "builds a ten-argument multiple-response mock" do
      m <- mock
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
      m <- mock (pure 42 :: Effect Int)
      let action = fun m

      m `hasBeenRunTimes` 0

      value <- liftEffect action
      value `shouldEqual` 42
      m `hasBeenRunTimes` 1

    it "records every execution of the same Effect value" do
      m <- mock (pure "result" :: Effect String)
      let action = fun m

      _ <- liftEffect action
      _ <- liftEffect action

      m `hasBeenRunTimes` 2

    it "records an execution even when the Effect fails" do
      m <- mock (throw "boom" :: Effect Unit)

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
      m <- mock
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

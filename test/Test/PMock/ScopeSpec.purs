module Test.PMock.ScopeSpec (scopeSpec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Test.PMock.Builder (mock)
import Test.PMock.Internal.Builder (MockCreation(..), build)
import Test.PMock.Expectation (called, once, with)
import Test.PMock.Param ((:>))
import Test.PMock.Scope (expects, withMock)
import Test.PMockSpecs (expectErrorWithMessage)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

scopeSpec :: Spec Unit
scopeSpec = describe "Scope" do
  it "automatically verifies expectations when the scope exits" do
    result <- liftEffect $ withMock do
      find <- mock ("Aja" :> 1977)
        `expects` (called once `with` "Aja")

      pure $ find "Aja"

    result `shouldEqual` 1977

  it "registers definition-time expectations without a function lookup" do
    result <- liftEffect $ withMock do
      find <- MockCreation (build Nothing ("Aja" :> 1977))
        `expects` (called once `with` "Aja")

      pure $ find "Aja"

    result `shouldEqual` 1977

  it "fails at scope exit when an expectation is not satisfied" do
    expectErrorWithMessage
      (joinWith "\n"
        [ "function was not called the expected number of times with the expected arguments."
        , "  expected arguments: \"Aja\""
        , "  expected count:     1"
        , "  but got count:      0"
        , ""
        , "  Call history (0 calls):"
        , "    (never called)"
        ])
      (liftEffect $ withMock do
        _ <- mock ("Aja" :> 1977)
          `expects` (called once `with` "Aja")
        pure unit)

  it "allows a scope without mocks" do
    result <- liftEffect $ withMock $ pure 42

    result `shouldEqual` 42

  it "keeps nested scopes isolated" do
    result <- liftEffect $ withMock do
      outer <- mock ("outer" :> 1)
        `expects` (called once `with` "outer")
      innerResult <- withMock do
        inner <- mock ("inner" :> 2)
          `expects` (called once `with` "inner")
        pure $ inner "inner"
      pure $ innerResult + outer "outer"

    result `shouldEqual` 3

  it "preserves an error from the scoped action" do
    expectErrorWithMessage "action failed"
      (liftEffect $ withMock $ throw "action failed")

  it "preserves the action error when verification also fails" do
    expectErrorWithMessage "action failed"
      (liftEffect $ withMock do
        _ <- mock ("Aja" :> 1977)
          `expects` (called once `with` "Aja")
        throw "action failed")

  it "rejects expectations outside a scope" do
    expectErrorWithMessage "expects must be used inside withMock."
      (liftEffect $
        mock ("Aja" :> 1977)
          `expects` (called once `with` "Aja"))

  it "verifies multiple mocks in the same scope" do
    result <- liftEffect $ withMock do
      findYear <- mock ("Aja" :> 1977)
        `expects` (called once `with` "Aja")
      isFavorite <- mock ("Aja" :> true)
        `expects` (called once `with` "Aja")

      pure { year: findYear "Aja", favorite: isFavorite "Aja" }

    result `shouldEqual` { year: 1977, favorite: true }

  it "verifies an argument-free Effect when the scope exits" do
    result <- liftEffect $ withMock do
      load <- mock (pure 42 :: Effect Int)
        `expects` called once

      load

    result `shouldEqual` 42

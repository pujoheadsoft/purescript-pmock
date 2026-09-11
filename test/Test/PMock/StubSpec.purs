module Test.PMock.StubSpec (stubSpec) where

import Prelude

import Data.String (joinWith)
import Effect (Effect)
import Effect.Class (liftEffect)
import Test.PMock.Builder (label)
import Test.PMock.Param (Param, any, matcher, (:>))
import Test.PMockSpecs (expectErrorWithMessage, runRuntimeThrowableFunction)
import Test.PMock.Stub (cases, onCase, stub)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

data Opaque = Opaque Int

stubSpec :: Spec Unit
stubSpec = describe "Stub" do
  it "builds a pure function for a single case" do
    let find = stub $ "Aja" :> 1977

    find "Aja" `shouldEqual` 1977

  it "selects a return value from multiple cases" do
    let find = stub $ cases [ "Aja" :> 1977, "Gaucho" :> 1980 ]

    find "Aja" `shouldEqual` 1977
    find "Gaucho" `shouldEqual` 1980

  it "defines multiple cases with a DSL" do
    let
      find = stub do
        onCase $ "Aja" :> 1977
        onCase $ "Gaucho" :> 1980

    find "Aja" `shouldEqual` 1977
    find "Gaucho" `shouldEqual` 1980

  it "uses the first matching stub case when matchers overlap" do
    let
      find = stub do
        onCase $ any @String :> 1977
        onCase $ "Aja" :> 1978

    find "Aja" `shouldEqual` 1977
    find "Aja" `shouldEqual` 1977

  it "builds a function with multiple arguments" do
    let find = stub $ "Aja" :> 1977 :> true :> "found"

    find "Aja" 1977 true `shouldEqual` "found"

  it "builds a ten-argument function" do
    let f = stub $ 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> 9 :> 10 :> "result"

    f 1 2 3 4 5 6 7 8 9 10 `shouldEqual` "result"

  it "accepts a predicate matcher without Eq or Show" do
    let
      positive = matcher (\(Opaque n) -> n > 0) "positive opaque" :: Param Opaque
      classify = stub $ positive :> "positive"

    classify (Opaque 1) `shouldEqual` "positive"

  it "reports an unexpected argument without recording it" do
    let find = stub $ "Aja" :> 1977

    expectErrorWithMessage
      (joinWith "\n"
        [ "Error: function was not called with the expected arguments."
        , "  expected: \"Aja\""
        , "   but got: \"Gaucho\""
        , "             ^^^^^^^"
        ])
      (runRuntimeThrowableFunction \_ -> find "Gaucho")

  it "points to the differing part of an unexpected string argument" do
    let find = stub $ "hello world" :> 1977

    expectErrorWithMessage
      (joinWith "\n"
        [ "Error: function was not called with the expected arguments."
        , "  expected: \"hello world\""
        , "   but got: \"hello purescript\""
        , "                   ^^^^^^^^^^^"
        ])
      (runRuntimeThrowableFunction \_ -> find "hello purescript")

  it "includes a label in a named stub failure" do
    let find = stub (label "albumYear") ("Aja" :> 1977)

    expectErrorWithMessage
      (joinWith "\n"
        [ "Error: function `albumYear` was not called with the expected arguments."
        , "  expected: \"Aja\""
        , "   but got: \"Gaucho\""
        , "             ^^^^^^^"
        ])
      (runRuntimeThrowableFunction \_ -> find "Gaucho")

  it "shows the nearest expected case and its string diff" do
    let
      find = stub do
        onCase $ "Aja" :> 1977
        onCase $ "Gaucho" :> 1980

    expectErrorWithMessage
      (joinWith "\n"
        [ "Error: function was not called with the expected arguments."
        , "  expected one of the following:"
        , "    \"Aja\""
        , "    \"Gaucho\""
        , "  but got:"
        , "    \"Ajx\""
        , "       ^^"
        ])
      (runRuntimeThrowableFunction \_ -> find "Ajx")

  it "returns an argument-free Effect without adding recording" do
    let action = stub (pure 42 :: Effect Int)

    result <- liftEffect action
    result `shouldEqual` 42

  it "returns an Effect from an argument-taking function" do
    let save = stub $ "progress" :> (pure "saved" :: Effect String)

    result <- liftEffect $ save "progress"
    result `shouldEqual` "saved"

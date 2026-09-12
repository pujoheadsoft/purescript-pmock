module Test.PMock.PublicApiSpec (publicApiSpec) where

import Prelude

import Control.Monad.State (StateT, runStateT)
import Data.String (joinWith)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.PMock (Param, TimesSpec, andThen, any, anything, called, calledInPartialOrder, calledWith, expects, greaterThan, inOrderWith, inPartialOrderWith, label, lessThan, matcher, matcher_, mock, onCase, once, shouldBeCalled, times, with, withMock, (:>))
import Test.PMockSpecs (expectErrorWithMessage)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

data Opaque = Opaque Int

exactlyOnce :: TimesSpec
exactlyOnce = once

publicApiSpec :: Spec Unit
publicApiSpec = describe "Public API" do
  it "supports the recommended mock DSL from Test.PMock" do
    result <- liftEffect $ withMock do
      find <- mock ("Aja" :> 1977)
        `expects` (called once `with` "Aja")

      pure $ find "Aja"

    result `shouldEqual` 1977

  it "supports post-hoc verification with the same expectation DSL" do
    find <- mock $ any @String :> 1977
    find "Aja" `shouldEqual` 1977

    find `shouldBeCalled` (once `with` "Aja")

  it "supports MockCat-style post-hoc verification" do
    find <- mock $ any @String :> 1977
    find "Aja" `shouldEqual` 1977

    find `shouldBeCalled` (exactlyOnce `with` "Aja")

  it "supports all MockCat-style post-hoc verification forms" do
    find <- mock $ any @String :> 1977
    find "Aja" `shouldEqual` 1977
    find "middle" `shouldEqual` 1977
    find "Gaucho" `shouldEqual` 1977

    find `shouldBeCalled` "Aja"
    find `shouldBeCalled` times 3
    find `shouldBeCalled` calledWith "Gaucho"
    find `shouldBeCalled` anything
    find `shouldBeCalled` inOrderWith [ "Aja", "middle", "Gaucho" ]
    find `shouldBeCalled` inPartialOrderWith [ "Aja", "Gaucho" ]

  it "shows the closest call, its string diff, and the call history" do
    find <- mock $ any @String :> 1977
    find "goodbye" `shouldEqual` 1977
    find "hello purescript" `shouldEqual` 1977

    expectErrorWithMessage
      (joinWith "\n"
        [ "function was not called with the expected arguments."
        , ""
        , "  Closest match:"
        , "    expected: \"hello world\""
        , "     but got: \"hello purescript\""
        , "                   ^^^^^^^^^^^"
        , ""
        , "  Call history (2 calls):"
        , "              1. \"goodbye\""
        , "    [Closest] 2. \"hello purescript\""
        ])
      (find `shouldBeCalled` "hello world")

  it "shows a structural diff for a recorded PureScript record" do
    find <- mock $ any @{ age :: Int, name :: String } :> true
    find { age: 20, name: "Fagen" } `shouldEqual` true

    expectErrorWithMessage
      (joinWith "\n"
        [ "function was not called with the expected arguments."
        , ""
        , "  Closest match:"
        , "    expected: { age: 30, name: \"Fagen\" }"
        , "     but got: { age: 20, name: \"Fagen\" }"
        , "                   ^^^^^^^^^^^^^^^^^^^"
        , "  Specific difference in `age`:"
        , "    expected: 30"
        , "     but got: 20"
        , "              ^^"
        , ""
        , "  Call history (1 call):"
        , "    [Closest] 1. { age: 20, name: \"Fagen\" }"
        ])
      (find `shouldBeCalled` { age: 30, name: "Fagen" })

  it "shows the expected arguments and call history for a count mismatch" do
    find <- mock $ any @String :> 1977
    find "Aja" `shouldEqual` 1977
    find "Gaucho" `shouldEqual` 1977

    expectErrorWithMessage
      (joinWith "\n"
        [ "function was not called the expected number of times with the expected arguments."
        , "  expected arguments: \"Aja\""
        , "  expected count:     2"
        , "  but got count:      1"
        , ""
        , "  Call history (2 calls):"
        , "    [Matched] 1. \"Aja\""
        , "              2. \"Gaucho\""
        ])
      (find `shouldBeCalled` (times 2 `with` "Aja"))

  it "supports MockCat names except for Prelude's when" do
    let positive = matcher (\(Opaque n) -> n > 0) "positive opaque" :: Param Opaque
    let positiveWithoutDescription = matcher_ (\(Opaque n) -> n > 0) :: Param Opaque

    result <- liftEffect $ withMock do
      check <- mock (positive :> true)
        `expects` do
          called (greaterThan 0) `with` positiveWithoutDescription
          called (lessThan 2)
          calledInPartialOrder [ positive ]

      pure $ check (Opaque 1)

    result `shouldEqual` true

  it "supports predicate matchers without Eq or Show in the public DSL" do
    let positive = matcher (\(Opaque n) -> n > 0) "positive opaque" :: Param Opaque
    check <- mock $ positive :> true
    check (Opaque 1) `shouldEqual` true

    check `shouldBeCalled` (once `with` positive)

  it "verifies a total count without specifying arguments" do
    find <- mock $ "Aja" :> 1977
    find "Aja" `shouldEqual` 1977

    find `shouldBeCalled` once

  it "rejects an ordinary function during post-hoc verification" do
    expectErrorWithMessage
      (joinWith "\n"
        [ "Error: PMock verification failed."
        , ""
        , "The function passed to 'shouldBeCalled' could not be recognized as a registered mock."
        , ""
        , "Possible causes:"
        , "  1. You passed a wrapper function around the mock."
        , "  2. You passed a normal (non-mock) function."
        , ""
        , "Solution:"
        , "  - Pass the function returned directly by 'mock'."
        , "  - If a wrapper is necessary, declare expectations with 'expects' inside 'withMock'."
        ])
      ((\_ -> 1977) `shouldBeCalled` once)

  it "verifies calls in partial order" do
    find <- mock $ any @String :> 1977
    find "before" `shouldEqual` 1977
    find "Aja" `shouldEqual` 1977
    find "after" `shouldEqual` 1977

    find `shouldBeCalled` inPartialOrderWith [ "before", "after" ]

  it "points to the argument that violates exact call order" do
    find <- mock $ any @String :> 1977
    find "first" `shouldEqual` 1977
    find "second" `shouldEqual` 1977

    expectErrorWithMessage
      (joinWith "\n"
        [ "function was not called with the expected arguments in the expected order."
        , "  expected 2nd call: \"third\""
        , "   but got 2nd call: \"second\""
        , "                      ^^^^^^^"
        ])
      (find `shouldBeCalled` inOrderWith [ "first", "third" ])

  it "shows expected and actual partial call order" do
    find <- mock $ any @String :> 1977
    find "second" `shouldEqual` 1977
    find "first" `shouldEqual` 1977

    expectErrorWithMessage
      (joinWith "\n"
        [ "function was not called with the expected arguments in the expected order."
        , "  expected order:"
        , "    \"first\""
        , "    \"third\""
        , "  but got:"
        , "    \"second\""
        , "    \"first\""
        ])
      (find `shouldBeCalled` inPartialOrderWith [ "first", "third" ])

  it "reports an exact-order call count mismatch without internal terms" do
    find <- mock $ any @String :> 1977
    find "first" `shouldEqual` 1977

    expectErrorWithMessage
      (joinWith "\n"
        [ "function was not called with the expected arguments in the expected order (count mismatch)."
        , "  expected: 2"
        , "   but got: 1"
        ])
      (find `shouldBeCalled` inOrderWith [ "first", "second" ])

  it "defines sequential responses with the cases DSL" do
    find <- mock do
      onCase $ "Aja" :> 1977
        `andThen` 1978

    find "Aja" `shouldEqual` 1977
    find "Aja" `shouldEqual` 1978
    find "Aja" `shouldEqual` 1978

  it "uses only the first matching mock case" do
    find <- mock do
      onCase $ any @String :> 1977
        `andThen` 1979
      onCase $ "Aja" :> 1978

    find "Aja" `shouldEqual` 1977
    find "Aja" `shouldEqual` 1979
    find "Aja" `shouldEqual` 1979

  it "tracks sequential responses independently for each selected case" do
    let positive = matcher (\(Opaque n) -> n > 0) "positive opaque" :: Param Opaque
    check <- mock do
      onCase $ positive :> 1
        `andThen` 2
        `andThen` 3
      onCase $ any @Opaque :> 10
        `andThen` 20

    check (Opaque 1) `shouldEqual` 1
    check (Opaque (-1)) `shouldEqual` 10
    check (Opaque 1) `shouldEqual` 2
    check (Opaque (-1)) `shouldEqual` 20
    check (Opaque 1) `shouldEqual` 3

  it "preserves Multi Mock first-match behavior for duplicate arguments" do
    find <- mock
      [ "Aja" :> 1977
      , "Aja" :> 1978
      ]

    find "Aja" `shouldEqual` 1977
    find "Aja" `shouldEqual` 1977

  it "keeps distinct cases independent" do
    find <- mock do
      onCase $ "Aja" :> 1977
        `andThen` 1978
      onCase $ "Gaucho" :> 1980
        `andThen` 1981

    find "Aja" `shouldEqual` 1977
    find "Gaucho" `shouldEqual` 1980
    find "Aja" `shouldEqual` 1978
    find "Gaucho" `shouldEqual` 1981

  it "supports sequential responses for functions with multiple arguments" do
    find <- mock do
      onCase $ "Aja" :> 1977 :> "first"
        `andThen` "second"

    find "Aja" 1977 `shouldEqual` "first"
    find "Aja" 1977 `shouldEqual` "second"
    find "Aja" 1977 `shouldEqual` "second"

  it "does not require Eq or Show for sequential return values" do
    next <- mock do
      onCase $ unit :> Opaque 1
        `andThen` Opaque 2

    case next unit of
      Opaque value -> value `shouldEqual` 1
    case next unit of
      Opaque value -> value `shouldEqual` 2

  it "supports labeled sequential responses" do
    find <- mock (label "albumYear") do
      onCase $ "Aja" :> 1977
        `andThen` 1978

    find "Aja" `shouldEqual` 1977
    find "Aja" `shouldEqual` 1978

  it "preserves arbitrary monadic return values" do
    find <- mock $ "Article Id" :> pure @Aff { title: "Article Title" }
    article <- find "Article Id"
    article `shouldEqual` { title: "Article Title" }

    update <- mock $
      "New Title" :> pure @(StateT { title :: String } Aff) unit
    _ <- runStateT (update "New Title") { title: "Old Title" }
    update `shouldBeCalled` (once `with` "New Title")

  it "labels a mock without requiring a separate constructor" do
    expectErrorWithMessage
      (joinWith "\n"
        [ "function `albumYear` was not called the expected number of times."
        , "  expected: 1"
        , "   but got: 0"
        ])
      (liftEffect $ withMock do
        _ <- mock (label "albumYear") ("Aja" :> 1977)
          `expects` called once
        pure unit)

  it "returns a directly callable function from a labeled mock" do
    find <- mock (label "albumYear") ("Aja" :> 1977)
    find "Aja" `shouldEqual` 1977
    find `shouldBeCalled` (once `with` "Aja")

  it "defines sequential argument-free Effects with the cases DSL" do
    load <- mock do
      onCase $ (pure "first" :: Effect String)
        `andThen` (pure "second" :: Effect String)

    first <- liftEffect load
    second <- liftEffect load
    last <- liftEffect load

    first `shouldEqual` "first"
    second `shouldEqual` "second"
    last `shouldEqual` "second"
    load `shouldBeCalled` times 3

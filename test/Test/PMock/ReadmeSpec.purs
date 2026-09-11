module Test.PMock.ReadmeSpec (readmeSpec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.PMock
  ( Param
  , any
  , called
  , calledWith
  , expects
  , inOrderWith
  , label
  , matcher
  , matcherBy
  , mock
  , never
  , notEqual
  , onCase
  , once
  , or
  , stub
  , times
  , shouldBeCalled
  , with
  , withMock
  , (:>)
  )
import Test.PMockSpecs (expectErrorWithMessage, runRuntimeThrowableFunction)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type Article =
  { id :: String
  , title :: String
  }

data Command = Save String

type AlbumDependencies =
  { findYear :: String -> Int
  , notify :: Effect Unit
  }

describeAlbum :: AlbumDependencies -> String -> Effect String
describeAlbum dependencies title = do
  dependencies.notify
  pure $ title <> " (" <> show (dependencies.findYear title) <> ")"

readmeSpec :: Spec Unit
readmeSpec = describe "README examples" do
  it "describes an album with stubs" do
    let
      dependencies =
        { findYear: stub $ "Aja" :> 1977
        , notify: stub (pure unit :: Effect Unit)
        }

    result <- liftEffect $ describeAlbum dependencies "Aja"

    result `shouldEqual` "Aja (1977)"

  it "notifies when describing an album" do
    notify <- mock (pure unit :: Effect Unit)
    let
      dependencies =
        { findYear: stub $ "Aja" :> 1977
        , notify
        }

    result <- liftEffect $ describeAlbum dependencies "Aja"

    result `shouldEqual` "Aja (1977)"
    notify `shouldBeCalled` once

  it "uses a pure stub without recording calls" do
    let findYear = stub $ "Aja" :> 1977

    findYear "Aja" `shouldEqual` 1977

  it "fails immediately for an undefined stub input" do
    let findYear = stub $ "Aja" :> 1977

    expectErrorWithMessage
      (joinWith "\n"
        [ "Error: function was not called with the expected arguments."
        , "  expected: \"Aja\""
        , "   but got: \"Gaucho\""
        , "             ^^^^^^^"
        ])
      (runRuntimeThrowableFunction \_ -> findYear "Gaucho")

  it "declares and automatically verifies a mock" do
    result <- liftEffect $ withMock do
      findYear <- mock ("Aja" :> 1977)
        `expects` (called once `with` "Aja")

      pure $ findYear "Aja"

    result `shouldEqual` 1977

  it "declares multiple expectations for one mock" do
    liftEffect $ withMock do
      save <- mock (any @String :> (pure unit :: Effect Unit))
        `expects` do
          called once `with` "article-1"
          called never `with` "missing"

      save "article-1"

  it "verifies a mock after an Aff computation" do
    findYear <- mock $
      any @String :> (pure 1977 :: Aff Int)

    year <- findYear "Aja"

    year `shouldEqual` 1977
    findYear `shouldBeCalled` (once `with` "Aja")

  it "replaces effectful application ports" do
    let article = { id: "article-1", title: "PMock 1.0" }

    result <- liftEffect $ withMock do
      findArticle <- mock
        ("article-1" :> (pure (Just article) :: Effect (Maybe Article)))
        `expects` (called once `with` "article-1")
      saveArticle <- mock
        (article :> (pure unit :: Effect Unit))
        `expects` (called once `with` article)
      notify <- mock (pure unit :: Effect Unit)
        `expects` called once

      found <- findArticle "article-1"
      saveArticle article
      notify
      pure found

    result `shouldEqual` Just article

  it "defines sequential responses with onCase" do
    next <- mock do
      onCase $ unit :> 1
      onCase $ unit :> 2

    next unit `shouldEqual` 1
    next unit `shouldEqual` 2
    next unit `shouldEqual` 2
    next `shouldBeCalled` times 3

  it "shares a sequential response across calls matching the same cases" do
    next <- mock do
      onCase $ any @String :> 1
      onCase $ any @String :> 2

    next "A" `shouldEqual` 1
    next "B" `shouldEqual` 2
    next "A" `shouldEqual` 2

  it "uses the first matching definition in a Multi Mock" do
    firstMatch <- mock
      [ any @String :> 1
      , "A" :> 2
      ]

    firstMatch "A" `shouldEqual` 1
    firstMatch "A" `shouldEqual` 1

  it "uses predicate matchers without Eq or Show" do
    let
      nonEmptyCommand =
        matcher
          (\(Save value) -> value /= "")
          "a non-empty Save"
        :: Param Command
    handle <- mock $ nonEmptyCommand :> unit

    handle (Save "article-1") `shouldEqual` unit
    handle `shouldBeCalled` (once `with` nonEmptyCommand)
    handle `shouldBeCalled`
      (never `with` matcher
        (\(Save value) -> value == "missing")
        "Save missing")

  it "customizes matcher diagnostics with matcherBy" do
    let
      positive = matcherBy
        { matches: \value -> value > 0
        , renderExpected: "a positive number"
        , renderActual: Just \value -> "number " <> show value
        }
      classify = stub $ positive :> "positive"

    classify 1 `shouldEqual` "positive"

  it "combines matchers" do
    let
      classify = stub $ (1 `or` 2 `or` 3) :> "small"
      exceptFive = stub $ notEqual 5 :> "not five"

    classify 2 `shouldEqual` "small"
    exceptFive 4 `shouldEqual` "not five"

  it "verifies recorded calls after execution" do
    output <- mock $ any @String :> unit

    output "first" `shouldEqual` unit
    output "second" `shouldEqual` unit

    output `shouldBeCalled` times 2

  it "verifies calls to a function with multiple arguments" do
    save <- mock $ any @String :> any @Int :> unit

    save "Aja" 1977 `shouldEqual` unit
    save "Gaucho" 1980 `shouldEqual` unit

    save `shouldBeCalled` (once `with` ("Aja" :> 1977))
    save `shouldBeCalled` calledWith ("Gaucho" :> 1980)
    save `shouldBeCalled` inOrderWith
      [ "Aja" :> 1977
      , "Gaucho" :> 1980
      ]

  it "records an argument-taking mock before its returned Effect runs" do
    save <- mock $ any @String :> (pure unit :: Effect Unit)

    let action = save "progress"
    save `shouldBeCalled` once

    liftEffect action
    save `shouldBeCalled` once

  it "shows the labeled diagnostic used in the README" do
    findYear <- mock (label "findYear") (any @String :> 1977)

    findYear "goodbye" `shouldEqual` 1977
    findYear "hello purescript" `shouldEqual` 1977

    expectErrorWithMessage
      (joinWith "\n"
        [ "function `findYear` was not called with the expected arguments."
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
      (findYear `shouldBeCalled` "hello world")

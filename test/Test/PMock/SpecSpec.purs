module Test.PMock.SpecSpec (specSpec) where

import Prelude

import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (attempt)
import Effect.Exception (message)
import Test.PMock (stub, (:>))
import Test.PMock.Spec (mockIt)
import Test.Spec (Item(..), Spec, Tree(..), collect, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

specSpec :: Spec Unit
specSpec = describe "Spec" do
  mockIt "runs a purescript-spec test" \_ -> do
    (1 + 1) `shouldEqual` 2

  it "reports a synchronous stub exception as a test failure" do
    let
      failingSpec = mockIt "find an album year" \_ -> do
        let findYear = stub $ "Aja" :> 1977
        findYear "Gaucho" `shouldEqual` 1980

    result <- case collect failingSpec of
      Identity [ Leaf _ (Just (Item item)) ] ->
        attempt $ item.example (\action -> action unit)
      _ -> do
        fail "mockIt did not create exactly one test"
        pure $ Right unit

    case result of
      Left runtimeError -> message runtimeError `shouldEqual`
        "Error: function was not called with the expected arguments.\n  expected: \"Aja\"\n   but got: \"Gaucho\"\n             ^^^^^^^"
      Right _ -> fail "mockIt did not report the synchronous exception"

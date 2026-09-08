module Test.PMock.MessageSpec (messageSpec) where

import Prelude

import Data.Maybe (Maybe(..))
import Test.PMock.Message (message, messageForMultiMock, mockNameLabel)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

messageSpec :: Spec Unit
messageSpec = describe "Message" do
  it "formats anonymous and named mock labels" do
    mockNameLabel Nothing `shouldEqual` " "
    mockNameLabel (Just "find") `shouldEqual` " `find` "

  it "formats a single expectation mismatch" do
    message (Just "find") "expected" "actual"
      `shouldEqual`
        "function `find` was not called with expected arguments.\n  expected: \"expected\"\n  but was : \"actual\""

  it "formats a multiple-expectation mismatch" do
    messageForMultiMock Nothing ["a", "b"] "c"
      `shouldEqual`
        "function was not called with expected arguments.\n  expected one of the following:\n    \"a\"\n    \"b\"\n  but was actual:\n    \"c\""

module Test.PMock.Internal.MessageSpec (messageSpec) where

import Prelude

import Data.Maybe (Maybe(..))
import Test.PMock.Internal.Message
  ( countMismatchMessage
  , countWithArgumentsMismatchMessage
  , detailedArgumentMismatchMessage
  , exactOrderCountMismatchMessage
  , message
  , messageForMultiMock
  , messageFromRendered
  , mockNameLabel
  , orderMismatchMessage
  , partialOrderCountMismatchMessage
  , partialOrderMismatchMessage
  )
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
        "function `find` was not called with the expected arguments.\n  expected: \"expected\"\n   but got: \"actual\"\n             ^^^^^^^^^"

  it "identifies a differing record field" do
    messageFromRendered Nothing
      "{ age: 30, name: \"Fagen\" }"
      "{ age: 20, name: \"Fagen\" }"
      `shouldEqual`
        "function was not called with the expected arguments.\n  Specific difference in `age`:\n    expected: 30\n     but got: 20\n              ^^\n\nFull context:\n  expected: { age: 30, name: \"Fagen\" }\n   but got: { age: 20, name: \"Fagen\" }\n                   ^^^^^^^^^^^^^^^^^^^"

  it "includes structural differences for the closest recorded call" do
    detailedArgumentMismatchMessage Nothing
      "{ age: 30, name: \"Fagen\" }"
      [ "{ age: 20, name: \"Fagen\" }" ]
      `shouldEqual`
        "function was not called with the expected arguments.\n\n  Closest match:\n    expected: { age: 30, name: \"Fagen\" }\n     but got: { age: 20, name: \"Fagen\" }\n                   ^^^^^^^^^^^^^^^^^^^\n  Specific difference in `age`:\n    expected: 30\n     but got: 20\n              ^^\n\n  Call history (1 call):\n    [Closest] 1. { age: 20, name: \"Fagen\" }"

  it "does not draw a meaningless diff for an unrendered actual value" do
    detailedArgumentMismatchMessage Nothing "positive opaque" [ "<actual value>" ]
      `shouldEqual`
        "function was not called with the expected arguments.\n\n  Closest match:\n    expected: positive opaque\n     but got: <actual value>\n\n  Call history (1 call):\n    [Closest] 1. <actual value>"

  it "identifies a field inside an ADT record rendering" do
    messageFromRendered Nothing
      "User { age: 30, name: \"Fagen\" }"
      "User { age: 20, name: \"Fagen\" }"
      `shouldEqual`
        "function was not called with the expected arguments.\n  Specific difference in `age`:\n    expected: 30\n     but got: 20\n              ^^\n\nFull context:\n  expected: User { age: 30, name: \"Fagen\" }\n   but got: User { age: 20, name: \"Fagen\" }\n                        ^^^^^^^^^^^^^^^^^^^"

  it "formats a multiple-expectation mismatch" do
    messageForMultiMock Nothing ["a", "b"] "c"
      `shouldEqual`
        "function was not called with the expected arguments.\n  expected one of the following:\n    \"a\"\n    \"b\"\n  but got:\n    \"c\"\n     ^^"

  it "distinguishes total and argument-specific count mismatches" do
    countMismatchMessage Nothing "1" 0
      `shouldEqual`
        "function was not called the expected number of times.\n  expected: 1\n   but got: 0"
    countWithArgumentsMismatchMessage (Just "find") "2" 1
      `shouldEqual`
        "function `find` was not called the expected number of times with the expected arguments.\n  expected: 2\n   but got: 1"

  it "formats exact-order differences" do
    orderMismatchMessage (Just "save")
      [ { position: 1, expected: "\"first\"", actual: "\"second\"" }
      , { position: 2, expected: "\"second\"", actual: "\"first\"" }
      ]
      `shouldEqual`
        "function `save` was not called with the expected arguments in the expected order.\n  expected 1st call: \"first\"\n   but got 1st call: \"second\"\n                      ^^^^^^^\n  expected 2nd call: \"second\"\n   but got 2nd call: \"first\"\n                      ^^^^^^^"

  it "formats partial-order differences" do
    partialOrderMismatchMessage Nothing ["\"first\"", "\"third\""]
      ["\"second\"", "\"first\""]
      `shouldEqual`
        "function was not called with the expected arguments in the expected order.\n  expected order:\n    \"first\"\n    \"third\"\n  but got:\n    \"second\"\n    \"first\""

  it "formats exact and partial order count mismatches" do
    exactOrderCountMismatchMessage Nothing 2 3
      `shouldEqual`
        "function was not called with the expected arguments in the expected order (count mismatch).\n  expected: 3\n   but got: 2"
    partialOrderCountMismatchMessage (Just "save") 1 2
      `shouldEqual`
        "function `save` was not called with the expected arguments in the expected order (count mismatch).\n  expected: 2\n   but got: 1"

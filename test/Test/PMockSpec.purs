module Test.PMockSpec (pmockSpec) where

import Prelude

import Control.Monad.Except (class MonadError)
import Control.Monad.State (StateT, runStateT)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Effect.Aff (Aff, Error)
import Test.PMock.Internal.Builder (fun, mockFun, mockHandle, namedMock)
import Test.PMock.Param (and, any, matcher, notEqual, or, (:>))
import Test.PMock.Internal.Verify (VerifyMatchType(..), hasBeenCalledInOrder, hasBeenCalledInPartialOrder, hasBeenCalledTimes, hasBeenCalledWith, with)
import Test.PMock.Spec (mockIt)
import Test.PMockSpecs (expectErrorWithMessage, runRuntimeThrowableFunction)
import Test.Spec (Spec, SpecT, describe, it)
import Test.Spec.Assertions (expectError, shouldEqual)

type Fixture mock r m = {
  name :: String,
  create :: Unit -> m mock,
  execute :: mock -> r,
  executeFailed :: Maybe (mock -> r),
  expected :: r,
  verifyMock :: mock -> m Unit,
  verifyCount :: mock -> Int -> m Unit,
  verifyFailed :: mock -> m Unit
}

type VerifyOrderFixture mock r m = {
  name :: String,
  create :: Unit -> m mock,
  execute :: mock -> r,
  verifyMock :: mock -> m Unit,
  verifyFailed :: mock -> m Unit
}

-- mock test template
mockTest :: forall mock m g r. Monad m => Eq r => Show r => MonadError Error g => Fixture mock r g -> SpecT g Unit m Unit
mockTest f = describe f.name do
  it "Returns a set value when called with a set argument." do
    m <- f.create unit
    f.execute m `shouldEqual` f.expected

  it "Failure to call with set arguments." do
    case f.executeFailed of
      Just func -> do
        m <- f.create unit
        expectError $ runRuntimeThrowableFunction (\_ -> func m)
      Nothing -> pure unit

  it "that the call was made with the set arguments." do
    m <- f.create unit
    let _ = f.execute m
    f.verifyMock m

  it "fails if the call is made with arguments different from those set." do
    m <- f.create unit
    let _ = f.execute m
    expectError $ f.verifyFailed m

  it "the number of times it has been called with the set arguments (0 times)." do
    m <- f.create unit
    f.verifyCount m 0

  it "the number of times it has been called with the set arguments (3 times)." do
    m <- f.create unit
    let
      _ = f.execute m
      _ = f.execute m
      _ = f.execute m
    f.verifyCount m 3

mockOrderTest :: forall mock m g r. Monad m => Eq r => Show r => MonadError Error g => VerifyOrderFixture mock r g -> SpecT g Unit m Unit
mockOrderTest f = describe f.name do
  it "call was made with set order." do
    m <- f.create unit
    let 
      _ = f.execute m
    f.verifyMock m

  it "fails if call with order different set order." do
    m <- f.create unit
    let 
      _ = f.execute m
    expectError $ f.verifyFailed m

pmockSpec :: Spec Unit
pmockSpec = do
  describe "PMock Test" do
    describe "Single-response mock" do

      mockTest {
        name: "behavior",
        create: \_ -> mockHandle $ "1" :> 1,
        expected: 1, 
        execute: \m -> fun m "1",
        executeFailed: Just \m -> fun m "2",
        verifyMock: \m -> m `hasBeenCalledWith` "1",
        verifyCount: \m c -> m `hasBeenCalledTimes` c `with` "1",
        verifyFailed: \m -> m `hasBeenCalledWith` "2"
      }

    describe "Multiple-response mock" do
      mockTest {
        name: "behavior",
        create: \_ -> mockHandle $ [
          "1" :> 10, 
          "2" :> 20
        ],
        expected: [
          10, 
          20
        ], 
        execute: \m -> [
          fun m "1", fun m "2"
        ],
        executeFailed: Just \m -> [ fun m "3" ],
        verifyMock: \m -> do 
          m `hasBeenCalledWith` "1"
          m `hasBeenCalledWith` "2"
        ,
        verifyCount: \m c -> do
          m `hasBeenCalledTimes` c `with` "1"
          m `hasBeenCalledTimes` c `with` "2"
        ,
        verifyFailed: \m -> m `hasBeenCalledWith` "3"
      }

    describe "Matcher" do
      mockTest {
        name: "Handling Arbitrary Arguments.", 
        create: \_ -> mockHandle $ any :> 11,
        expected: [11, 11, 11], 
        execute: \m -> [fun m "1233", fun m "1234", fun m "2234"],
        executeFailed: Nothing,
        verifyMock: \m -> m `hasBeenCalledWith` "1234",
        verifyCount: \m c -> m `hasBeenCalledTimes` c $ "1234",
        verifyFailed: \m -> m `hasBeenCalledWith` "foo"
      }

      mockTest {
        name: "with arbitrary arguments", 
        create: \_ -> mockHandle $ "1234" :> 11,
        expected: 11, 
        execute: \m -> fun m "1234",
        executeFailed: Just \m -> fun m "1233",
        verifyMock: \m -> m `hasBeenCalledWith` (any @String),
        verifyCount: \m c -> m `hasBeenCalledTimes` c $ (any @String),
        verifyFailed: \m -> m `hasBeenCalledWith` "not called param"
      }

      mockTest {
        name: "Handling arguments with your own Matcher.", 
        create: \_ -> mockHandle $ matcher (_ > 10) "> 10" :> "Expected",
        expected: "Expected", 
        execute: \m -> fun m 11,
        executeFailed: Just \m -> fun m 10,
        verifyMock: \m -> m `hasBeenCalledWith` 11,
        verifyCount: \m c -> m `hasBeenCalledTimes` c $ 11,
        verifyFailed: \m -> m `hasBeenCalledWith` 10
      }

      mockTest {
        name: "arguments with your own Matcher", 
        create: \_ -> mockHandle $ 10 :> "Expected",
        expected: "Expected", 
        execute: \m -> fun m 10,
        executeFailed: Just \m -> fun m 1000,
        verifyMock: \m -> m `hasBeenCalledWith` matcher (_ < 11) "< 11",
        verifyCount: \m c -> m `hasBeenCalledTimes` c $ matcher (_ > 9) "> 9",
        verifyFailed: \m -> m `hasBeenCalledWith` matcher (_ > 11) "> 11"
      }

      mockTest {
        name: "Handling Logical Matcher Or.", 
        create: \_ -> mockHandle $ "a" `or` "b" `or` "c" :> 111,
        expected: [111, 111, 111], 
        execute: \m -> [fun m "a", fun m "b", fun m "c"],
        executeFailed: Just \m -> [fun m "d"],
        verifyMock: \m -> m `hasBeenCalledWith` "a",
        verifyCount: \m c -> m `hasBeenCalledTimes` c $ "b",
        verifyFailed: \m -> m `hasBeenCalledWith` "d"
      }

      mockTest {
        name: "Handling Logical Matcher And.", 
        create: \_ -> mockHandle $ (matcher (_ >= 5) ">= 5") `and` (matcher (_ <= 7) "<= 7") :> true :> 10,
        expected: [10, 10, 10], 
        execute: \m -> [fun m 5 true, fun m 6 true, fun m 7 true],
        executeFailed: Just \m -> [fun m 8 true],
        verifyMock: \m -> m `hasBeenCalledWith` (5 :> true),
        verifyCount: \m c -> m `hasBeenCalledTimes` c $ 6 :> true,
        verifyFailed: \m -> m `hasBeenCalledWith` (8 :> true)
      }

      mockTest {
        name: "Include Not Matcher as an argument.",
        create: \_ -> mockHandle $ notEqual "X" :> 11,
        expected: 11,
        execute: \m -> fun m "x",
        executeFailed: Just \m -> fun m "X",
        verifyMock: \m -> m `hasBeenCalledWith` "x",
        verifyCount: \m c -> m `hasBeenCalledTimes` c $ "x",
        verifyFailed: \m -> m `hasBeenCalledWith` "X"
      }

      mockTest {
        name: "with Not Matcher.",
        create: \_ -> mockHandle $ "X" :> 11,
        expected: 11,
        execute: \m -> fun m "X",
        executeFailed: Nothing,
        verifyMock: \m -> m `hasBeenCalledWith` notEqual "x",
        verifyCount: \m c -> m `hasBeenCalledTimes` c $ notEqual "x",
        verifyFailed: \m -> m `hasBeenCalledWith` "x"
      }

      mockTest {
        name: "Include Not Matcher (function) as an argument.",
        create: \_ -> mockHandle $ (notEqual $ matcher (_ > 10) "> 10") :> 11,
        expected: 11,
        execute: \m -> fun m 10,
        executeFailed: Just \m -> fun m 11,
        verifyMock: \m -> m `hasBeenCalledWith` 10,
        verifyCount: \m c -> m `hasBeenCalledTimes` c $ 10,
        verifyFailed: \m -> m `hasBeenCalledWith` 11
      }

      it "Arbitrary Arguments All Match Arg1" do
        m <- mockHandle $ any :> 100
        let
          _ = fun m 30
          _ = fun m 40

        m `hasBeenCalledWith` (MatchAll $ matcher (_ >= 30) ">= 30")

      it "Arbitrary Arguments All Match Arg2" do
        m <- mockHandle $ "Title" :> any :> false
        let
          _ = fun m "Title" 2020
          _ = fun m "Title" 2001

        m `hasBeenCalledWith` (MatchAll $ "Title" :> matcher (_ > 2000) "> 2000")

    describe "Order Verification" do
      describe "exactly sequential order." do
        mockOrderTest {
          name: "1 Arguments", 
          create: \_ -> mockHandle $ any :> unit,
          execute: \m -> do
            let
              _ = fun m "a"
              _ = fun m "b"
              _ = fun m "c"
            unit,
          verifyMock: \m -> m `hasBeenCalledInOrder` [
            "a",
            "b",
            "c"
          ],
          verifyFailed: \m -> m `hasBeenCalledInOrder` [
            "a",
            "b",
            "b"
          ]
        }

        mockOrderTest {
          name: "2 Arguments", 
          create: \_ -> mockHandle $ any :> any :> unit,
          execute: \m -> do
            let
              _ = fun m "a" 1
              _ = fun m "b" 2
              _ = fun m "c" 3
            unit,
          verifyMock: \m -> m `hasBeenCalledInOrder` [
            "a" :> 1,
            "b" :> 2,
            "c" :> 3
          ],
          verifyFailed: \m -> m `hasBeenCalledInOrder` [
            "a" :> 2,
            "b" :> 2,
            "c" :> 3
          ]
        }

        mockOrderTest {
          name: "number of function calls doesn't match the number of params", 
          create: \_ -> mockHandle $ any :> unit,
          execute: \m -> do
            let
              _ = fun m "a"
            unit,
          verifyMock: \m -> m `hasBeenCalledInOrder` [
            "a"
          ],
          verifyFailed: \m -> m `hasBeenCalledInOrder` [
            "a",
            "b"
          ]
        }
      
      describe "partially sequential order." do
        mockOrderTest {
          name: "1 Arguments", 
          create: \_ -> mockHandle $ any :> unit,
          execute: \m -> do
            let
              _ = fun m "a"
              _ = fun m "b"
              _ = fun m "c"
            unit,
          verifyMock: \m -> m `hasBeenCalledInPartialOrder` [
            "a",
            "c"
          ],
          verifyFailed: \m -> m `hasBeenCalledInPartialOrder` [
            "b",
            "a"
          ]
        }

        mockOrderTest {
          name: "2 Arguments", 
          create: \_ -> mockHandle $ any :> any :> unit,
          execute: \m -> do
            let
              _ = fun m "a" true
              _ = fun m "b" false
              _ = fun m "c" true
            unit,
          verifyMock: \m -> m `hasBeenCalledInPartialOrder` [
            "a" :> true,
            "c" :> true
          ],
          verifyFailed: \m -> m `hasBeenCalledInPartialOrder` [
            "b" :> false,
            "a" :> true
          ]
        }

        mockOrderTest {
          name: "Uncalled value specified.", 
          create: \_ -> mockHandle $ any :> unit,
          execute: \m -> do
            let
              _ = fun m "a"
              _ = fun m "b"
              _ = fun m "c"
            unit,
          verifyMock: \m -> m `hasBeenCalledInPartialOrder` [
            "b",
            "c"
          ],
          verifyFailed: \m -> m `hasBeenCalledInPartialOrder` [
            "a",
            "d"
          ]
        }

        mockOrderTest {
          name: "number of function calls doesn't match the number of params", 
          create: \_ -> mockHandle $ any :> unit,
          execute: \m -> do
            let
              _ = fun m "a"
            unit,
          verifyMock: \m -> m `hasBeenCalledInPartialOrder` [
            "a"
          ],
          verifyFailed: \m -> m `hasBeenCalledInPartialOrder` [
            "a",
            "b"
          ]
        }

    describe "Utility" do
      it "Create mock functions directly." do
        fn <- mockFun $ "a" :> true :> 300
        fn "a" true `shouldEqual` 300
      mockIt "Supplemental runtime exceptions `it`" \_ -> do
        m <- mockHandle $ 1 :> 2
        -- If you change the following values to values different from the expected values, you will see that you have supplemented the exception.
        fun m 1 `shouldEqual` 2

    -- Type annotation is required depending on the monad to be returned.
    describe "Monad" do
      it "Return Monad." do
        m <- mockHandle $ "Article Id" :> pure @Aff { title: "Article Title" }

        result <- fun m "Article Id"

        result `shouldEqual` {title: "Article Title"}
        
        m `hasBeenCalledWith` "Article Id"
      
      it "Return Monad(update)." do
        updateMock <- mockHandle $ "New Title" :> pure @(StateT State Aff) unit
        _ <- runStateT (fun updateMock "New Title") {article: {title: "Old Title"}} 
        updateMock `hasBeenCalledWith` "New Title"

    mockTest {
      name: "ADT", 
      create: \_ -> mockHandle $ (Data1 "data1") :> "data1",
      expected: "data1", 
      execute: \m -> fun m (Data1 "data1"),
      executeFailed: Just \m -> fun m (Data1 "data2"),
      verifyMock: \m -> m `hasBeenCalledWith` (Data1 "data1"),
      verifyCount: \m c -> m `hasBeenCalledTimes` c $ (Data1 "data1"),
      verifyFailed: \m -> m `hasBeenCalledWith` (Data2 "data1")
    }

  describe "Appropriate message when a test fails." do
    describe "anonymous mock" do
      describe "call" do
        it "simple mock"  do
          m <- mockHandle $ "a" :> 100
          let
            expected = joinWith "\n" [
              "Error: function was not called with the expected arguments.",
              "  expected: \"a\"",
              "   but got: \"b\"",
              "             ^^"
            ]
          expectErrorWithMessage expected $ runRuntimeThrowableFunction \_ -> fun m "b"

        it "multi mock" do
          m <- mockHandle [
            "aaa" :> 100 :> true,
            "bbb" :> 200 :> false
          ]
          let
            expected = joinWith "\n" [
              "Error: function was not called with the expected arguments.",
              "  expected one of the following:",
              "    \"aaa\",100",
              "    \"bbb\",200",
              "  but got:",
              "    \"aaa\",200",
              "          ^^^"
            ]
          expectErrorWithMessage expected $ runRuntimeThrowableFunction \_ -> fun m "aaa" 200

      describe "verify" do
        it "simple mock verify" do
          m <- mockHandle $ any@String :> 100
          let
            _ = fun m "A"
            expected = joinWith "\n" [
              "function was not called with the expected arguments.",
              "  expected: \"X\"",
              "   but got: \"A\"",
              "             ^^"
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledWith` "X"

        it "count" do
          m <- mockHandle $ any@String :> 100
          let
            _ = fun m "A"
            expected = joinWith "\n" [
              "function was not called the expected number of times with the expected arguments.",
              "  expected arguments: \"A\"",
              "  expected count:     2",
              "  but got count:      1",
              "",
              "  Call history (1 call):",
              "    [Matched] 1. \"A\""
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledTimes` 2 `with` "A"

        it "verifySequence" do
          m <- mockHandle $ any@String :> 100
          let
            _ = fun m "B"
            _ = fun m "C"
            _ = fun m "A"
            expected = joinWith "\n" [
              "function was not called with the expected arguments in the expected order.",
              "  expected 1st call: \"A\"",
              "   but got 1st call: \"B\"",
              "                      ^^",
              "  expected 2nd call: \"B\"",
              "   but got 2nd call: \"C\"",
              "                      ^^",
              "  expected 3rd call: \"C\"",
              "   but got 3rd call: \"A\"",
              "                      ^^"
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledInOrder` ["A", "B", "C"]
        
        it "verifySequence (count mismatch)" do
          m <- mockHandle $ any@String :> 100
          let
            _ = fun m "B"
            _ = fun m "C"
            expected = joinWith "\n" [
              "function was not called with the expected arguments in the expected order (count mismatch).",
              "  expected: 3",
              "   but got: 2"
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledInOrder` ["A", "B", "C"]
        
        it "verifyPartiallySequence" do
          m <- mockHandle $ any@String :> 100
          let
            _ = fun m "B"
            _ = fun m "A"
            expected = joinWith "\n" [
              "function was not called with the expected arguments in the expected order.",
              "  expected order:",
              "    \"A\"",
              "    \"C\"",
              "  but got:",
              "    \"B\"",
              "    \"A\""
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledInPartialOrder` ["A", "C"]

        it "verifyPartiallySequence (count mismatch)" do
          m <- mockHandle $ any@String :> 100
          let
            _ = fun m "B"
            expected = joinWith "\n" [
              "function was not called with the expected arguments in the expected order (count mismatch).",
              "  expected: 2",
              "   but got: 1"
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledInPartialOrder` ["A", "C"]

    describe "named mock" do
      describe "call" do
        it "simple mock"  do
          m <- namedMock "mock function" $ "a" :> 100
          let
            expected = joinWith "\n" [
              "Error: function `mock function` was not called with the expected arguments.",
              "  expected: \"a\"",
              "   but got: \"b\"",
              "             ^^"
            ]
          expectErrorWithMessage expected $ runRuntimeThrowableFunction \_ -> fun m "b"

        it "multi mock" do
          m <- namedMock "mock function" [
            "aaa" :> 100 :> true,
            "bbb" :> 200 :> false
          ]
          let
            expected = joinWith "\n" [
              "Error: function `mock function` was not called with the expected arguments.",
              "  expected one of the following:",
              "    \"aaa\",100",
              "    \"bbb\",200",
              "  but got:",
              "    \"aaa\",200",
              "          ^^^"
            ]
          expectErrorWithMessage expected $ runRuntimeThrowableFunction \_ -> fun m "aaa" 200

      describe "verify" do
        it "simple mock verify" do
          m <- namedMock "mock function" $ any@String :> 100
          let
            _ = fun m "A"
            expected = joinWith "\n" [
              "function `mock function` was not called with the expected arguments.",
              "  expected: \"X\"",
              "   but got: \"A\"",
              "             ^^"
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledWith` "X"

        it "count" do
          m <- namedMock "mock function" $ any@String :> 100
          let
            _ = fun m "A"
            expected = joinWith "\n" [
              "function `mock function` was not called the expected number of times with the expected arguments.",
              "  expected arguments: \"A\"",
              "  expected count:     2",
              "  but got count:      1",
              "",
              "  Call history (1 call):",
              "    [Matched] 1. \"A\""
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledTimes` 2 `with` "A"

        it "verifySequence" do
          m <- namedMock "mock function" $ any@String :> 100
          let
            _ = fun m "B"
            _ = fun m "C"
            _ = fun m "A"
            expected = joinWith "\n" [
              "function `mock function` was not called with the expected arguments in the expected order.",
              "  expected 1st call: \"A\"",
              "   but got 1st call: \"B\"",
              "                      ^^",
              "  expected 2nd call: \"B\"",
              "   but got 2nd call: \"C\"",
              "                      ^^",
              "  expected 3rd call: \"C\"",
              "   but got 3rd call: \"A\"",
              "                      ^^"
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledInOrder` ["A", "B", "C"]
        
        it "verifySequence (count mismatch)" do
          m <- namedMock "mockFunc" $ any@String :> 100
          let
            _ = fun m "B"
            _ = fun m "C"
            expected = joinWith "\n" [
              "function `mockFunc` was not called with the expected arguments in the expected order (count mismatch).",
              "  expected: 3",
              "   but got: 2"
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledInOrder` ["A", "B", "C"]
        
        it "verifyPartiallySequence" do
          m <- namedMock "mock function" $ any@String :> 100
          let
            _ = fun m "B"
            _ = fun m "A"
            expected = joinWith "\n" [
              "function `mock function` was not called with the expected arguments in the expected order.",
              "  expected order:",
              "    \"A\"",
              "    \"C\"",
              "  but got:",
              "    \"B\"",
              "    \"A\""
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledInPartialOrder` ["A", "C"]

        it "verifyPartiallySequence (count mismatch)" do
          m <- namedMock "mockFunc" $ any@String :> 100
          let
            _ = fun m "B"
            expected = joinWith "\n" [
              "function `mockFunc` was not called with the expected arguments in the expected order (count mismatch).",
              "  expected: 2",
              "   but got: 1"
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledInPartialOrder` ["A", "C"]

type Article = {
  title :: String
}

type State = { 
  article :: Article 
}

data Data a = Data1 a | Data2 a
derive instance genericData :: Generic (Data a) _
instance showData :: Show a => Show (Data a) where
  show = genericShow
instance eqData :: Eq a => Eq (Data a) where
  eq = genericEq

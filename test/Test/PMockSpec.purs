module Test.PMockSpec (pmockSpec) where

import Prelude

import Control.Monad.Except (class MonadError)
import Control.Monad.State (StateT, runStateT)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff, Error)
import Test.PMock (CountVerifyMethod(..), VerifyMatchType(..), and, any, fun, matcher, mock, mockFun, not, or, verify, verifyCount, verifyPartiallySequence, verifySequence, (:>))
import Test.PMockSpecs (mockIt, runRuntimeThrowableFunction)
import Test.Spec (Spec, SpecT, describe, it)
import Test.Spec.Assertions (expectError, shouldEqual)

type Fixture mock r m = {
  name :: String,
  create :: Unit -> mock,
  execute :: mock -> r,
  executeFailed :: Maybe (mock -> r),
  expected :: r,
  verifyMock :: mock -> m Unit,
  verifyCount :: mock -> Int -> m Unit,
  verifyFailed :: mock -> m Unit
}

type VerifyOrderFixture mock r m = {
  name :: String,
  create :: Unit -> mock,
  execute :: mock -> r,
  verifyMock :: mock -> m Unit,
  verifyFailed :: mock -> m Unit
}

-- mock test template
mockTest :: forall mock m g r. Monad m => Eq r => Show r => MonadError Error g => Fixture mock r g -> SpecT g Unit m Unit
mockTest f = describe f.name do
  it "Returns a set value when called with a set argument." do
    let m = f.create unit
    f.execute m `shouldEqual` f.expected

  it "Failure to call with set arguments." do
    case f.executeFailed of
      Just func -> let m = f.create unit
        in expectError $ runRuntimeThrowableFunction (\_ -> func m)
      Nothing -> pure unit

  it "Verify that the call was made with the set arguments." do
    let 
      m = f.create unit
      _ = f.execute m
    f.verifyMock m

  it "Verify fails if the call is made with arguments different from those set." do
    let 
      m = f.create unit
      _ = f.execute m
    expectError $ f.verifyFailed m

  it "Verify the number of times it has been called with the set arguments (0 times)." do
    let m = f.create unit
    f.verifyCount m 0

  it "Verify the number of times it has been called with the set arguments (3 times)." do
    let 
      m = f.create unit
      _ = f.execute m
      _ = f.execute m
      _ = f.execute m
    f.verifyCount m 3

mockOrderTest :: forall mock m g r. Monad m => Eq r => Show r => MonadError Error g => VerifyOrderFixture mock r g -> SpecT g Unit m Unit
mockOrderTest f = describe f.name do
  it "Verify call was made with set order." do
    let 
      m = f.create unit
      _ = f.execute m
    f.verifyMock m

  it "Verify fails if call with order different set order." do
    let 
      m = f.create unit
      _ = f.execute m
    expectError $ f.verifyFailed m

pmockSpec :: Spec Unit
pmockSpec = do
  describe "PMock Test" do
    describe "Single calls" do

      mockTest {
        name: "1 argument", 
        create: \_ -> mock $ "1" :> 1,
        expected: 1, 
        execute: \m -> fun m "1",
        executeFailed: Just \m -> fun m "2",
        verifyMock: \m -> verify m "1",
        verifyCount: \m c -> verifyCount m c "1",
        verifyFailed: \m -> verify m "2"
      }

      mockTest {
        name: "2 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true,
        expected: true, 
        execute: \m -> fun m 100 "1",
        executeFailed: Just \m -> fun m 100 "2",
        verifyMock: \m -> verify m $ 100 :> "1",
        verifyCount: \m c -> verifyCount m c $ 100 :> "1",
        verifyFailed: \m -> verify m $ 100 :> "2"
      }

      mockTest {
        name: "3 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1,
        expected: 11.1, 
        execute: \m -> fun m 100 "1" true,
        executeFailed: Just \m -> fun m 100 "1" false,
        verifyMock: \m -> verify m $ 100 :> "1" :> true,
        verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true,
        verifyFailed: \m -> verify m $ 100 :> "1" :> false
      }

      mockTest {
        name: "4 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2],
        expected: [1, 2], 
        execute: \m -> fun m 100 "1" true 11.1,
        executeFailed: Just \m -> fun m 100 "1" true 11.0,
        verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1,
        verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1,
        verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.0
      }

      mockTest {
        name: "5 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"},
        expected: {name: "Name"}, 
        execute: \m -> fun m 100 "1" true 11.1 [1, 2],
        executeFailed: Just \m -> fun m 100 "1" true 11.1 [1, 3],
        verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2],
        verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1 :> [1, 2],
        verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [2, 2]
      }

      mockTest {
        name: "6 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20,
        expected: 20, 
        execute: \m -> fun m 100 "1" true 11.1 [1, 2] {name: "Name"},
        executeFailed: Just \m -> fun m 100 "1" true 11.1 [1, 3] {name: "Nam"},
        verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"},
        verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"},
        verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Nome"}
      }

      mockTest {
        name: "7 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X",
        expected: "X", 
        execute: \m -> fun m 100 "1" true 11.1 [1, 2] {name: "Name"} 20,
        executeFailed: Just \m -> fun m 100 "1" true 11.1 [1, 3] {name: "Name"} 21,
        verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20,
        verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20,
        verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 19
      }

      mockTest {
        name: "8 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> false,
        expected: false, 
        execute: \m -> fun m 100 "1" true 11.1 [1, 2] {name: "Name"} 20 "X",
        executeFailed: Just \m -> fun m 100 "1" true 11.1 [1, 3] {name: "Name"} 20 "Y",
        verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X",
        verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X",
        verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "Z"
      }

      mockTest {
        name: "9 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> false :> 0.1,
        expected: 0.1, 
        execute: \m -> fun m 100 "1" true 11.1 [1, 2] {name: "Name"} 20 "X" false,
        executeFailed: Just \m -> fun m 100 "1" true 11.1 [1, 3] {name: "Name"} 20 "X" true,
        verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> false,
        verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> false,
        verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> true
      }

    describe "Multiple calls" do
      mockTest {
        name: "1 argument", 
        create: \_ -> mock $ [
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
          verify m "1"
          verify m "2"
        ,
        verifyCount: \m c -> do
          verifyCount m c "1"
          verifyCount m c "2"
        ,
        verifyFailed: \m -> verify m "3"
      }

      mockTest {
        name: "2 arguments", 
        create: \_ -> mock $ [
          "1" :> 10 :> true, 
          "2" :> 20 :> false
        ],
        expected: [
          true, 
          false
        ], 
        execute: \m -> [
          fun m "1" 10, 
          fun m "2" 20
        ],
        executeFailed: Just \m -> [ fun m "2" 10 ],
        verifyMock: \m -> do 
          verify m $ "1" :> 10
          verify m $ "2" :> 20
        ,
        verifyCount: \m c -> do
          verifyCount m c $ "1" :> 10
          verifyCount m c $ "2" :> 20
        ,
        verifyFailed: \m -> verify m $ "1" :> 30
      }

      mockTest {
        name: "3 arguments", 
        create: \_ -> mock $ [
          "1" :> 10 :> true  :> "a1", 
          "2" :> 20 :> false :> "a2"
        ],
        expected: [
          "a1", 
          "a2"
        ], 
        execute: \m -> [
          fun m "1" 10 true, 
          fun m "2" 20 false
        ],
        executeFailed: Just \m -> [ fun m "2" 20 true ],
        verifyMock: \m -> do 
          verify m $ "1" :> 10 :> true
          verify m $ "2" :> 20 :> false
        ,
        verifyCount: \m c -> do
          verifyCount m c $ "1" :> 10 :> true
          verifyCount m c $ "2" :> 20 :> false
        ,
        verifyFailed: \m -> verify m $ "1" :> 10 :> false
      }

      mockTest {
        name: "4 arguments", 
        create: \_ -> mock $ [
          "1" :> 10 :> true  :> "a1" :> 2.0, 
          "2" :> 20 :> false :> "a2" :> 3.0
        ],
        expected: [
          2.0, 
          3.0
        ], 
        execute: \m -> [
          fun m "1" 10 true  "a1", 
          fun m "2" 20 false "a2"
        ],
        executeFailed: Just \m -> [ fun m "2" 20 false "a1" ],
        verifyMock: \m -> do 
          verify m $ "1" :> 10 :> true  :> "a1"
          verify m $ "2" :> 20 :> false :> "a2"
        ,
        verifyCount: \m c -> do
          verifyCount m c $ "1" :> 10 :> true  :> "a1"
          verifyCount m c $ "2" :> 20 :> false :> "a2"
        ,
        verifyFailed: \m -> verify m $ "1" :> 10 :> true :> "a3"
      }

      mockTest {
        name: "5 arguments", 
        create: \_ -> mock $ [
          "1" :> 10 :> true  :> "a1" :> 2.0 :> false, 
          "2" :> 20 :> false :> "a2" :> 3.0 :> true
        ],
        expected: [
          false, 
          true
        ], 
        execute: \m -> [
          fun m "1" 10 true  "a1" 2.0, 
          fun m "2" 20 false "a2" 3.0
        ],
        executeFailed: Just \m -> [ fun m "2" 20 false "a2" 3.1 ],
        verifyMock: \m -> do 
          verify m $ "1" :> 10 :> true  :> "a1" :> 2.0
          verify m $ "2" :> 20 :> false :> "a2" :> 3.0
        ,
        verifyCount: \m c -> do
          verifyCount m c $ "1" :> 10 :> true  :> "a1" :> 2.0
          verifyCount m c $ "2" :> 20 :> false :> "a2" :> 3.0
        ,
        verifyFailed: \m -> verify m $ "1" :> 10 :> true :> "a1" :> 1.0
      }

      mockTest {
        name: "6 arguments", 
        create: \_ -> mock $ [
          "1" :> 10 :> true  :> "a1" :> 2.0 :> false :> "b2", 
          "2" :> 20 :> false :> "a2" :> 3.0 :> true  :> "b3"
        ],
        expected: [
          "b2", 
          "b3"
        ], 
        execute: \m -> [
          fun m "1" 10 true  "a1" 2.0 false, 
          fun m "2" 20 false "a2" 3.0 true
        ],
        executeFailed: Just \m -> [ fun m "2" 20 false "a2" 3.0 false ],
        verifyMock: \m -> do 
          verify m $ "1" :> 10 :> true  :> "a1" :> 2.0 :> false
          verify m $ "2" :> 20 :> false :> "a2" :> 3.0 :> true
        ,
        verifyCount: \m c -> do
          verifyCount m c $ "1" :> 10 :> true  :> "a1" :> 2.0 :> false
          verifyCount m c $ "2" :> 20 :> false :> "a2" :> 3.0 :> true
        ,
        verifyFailed: \m -> verify m $ "1" :> 10 :> true :> "a1" :> 2.0 :> true
      }

      mockTest {
        name: "7 arguments", 
        create: \_ -> mock $ [
          "1" :> 10 :> true  :> "a1" :> 2.0 :> false :> "b2" :> 200, 
          "2" :> 20 :> false :> "a2" :> 3.0 :> true  :> "b3" :> 300
        ],
        expected: [
          200, 
          300
        ], 
        execute: \m -> [
          fun m "1" 10 true  "a1" 2.0 false "b2", 
          fun m "2" 20 false "a2" 3.0 true  "b3"
        ],
        executeFailed: Just \m -> [ fun m "2" 20 false "a2" 3.0 true "b2" ],
        verifyMock: \m -> do 
          verify m $ "1" :> 10 :> true  :> "a1" :> 2.0 :> false :> "b2"
          verify m $ "2" :> 20 :> false :> "a2" :> 3.0 :> true  :> "b3"
        ,
        verifyCount: \m c -> do
          verifyCount m c $ "1" :> 10 :> true  :> "a1" :> 2.0 :> false :> "b2"
          verifyCount m c $ "2" :> 20 :> false :> "a2" :> 3.0 :> true  :> "b3"
        ,
        verifyFailed: \m -> verify m $ "1" :> 10 :> true :> "a1" :> 2.0 :> false :> "b3"
      }

      mockTest {
        name: "8 arguments", 
        create: \_ -> mock $ [
          "1" :> 10 :> true  :> "a1" :> 2.0 :> false :> "b2" :> 200 :> true,
          "2" :> 20 :> false :> "a2" :> 3.0 :> true  :> "b3" :> 300 :> false
        ],
        expected: [
          true, 
          false
        ], 
        execute: \m -> [
          fun m "1" 10 true  "a1" 2.0 false "b2" 200, 
          fun m "2" 20 false "a2" 3.0 true  "b3" 300
        ],
        executeFailed: Just \m -> [ fun m "2" 20 false "a2" 3.0 true "b3" 200 ],
        verifyMock: \m -> do 
          verify m $ "1" :> 10 :> true  :> "a1" :> 2.0 :> false :> "b2" :> 200
          verify m $ "2" :> 20 :> false :> "a2" :> 3.0 :> true  :> "b3" :> 300
        ,
        verifyCount: \m c -> do
          verifyCount m c $ "1" :> 10 :> true  :> "a1" :> 2.0 :> false :> "b2" :> 200
          verifyCount m c $ "2" :> 20 :> false :> "a2" :> 3.0 :> true  :> "b3" :> 300
        ,
        verifyFailed: \m -> verify m $ "1" :> 10 :> true :> "a1" :> 2.0 :> false :> "b2" :> 300
      }

      mockTest {
        name: "9 arguments", 
        create: \_ -> mock $ [
          "1" :> 10 :> true  :> "a1" :> 2.0 :> false :> "b2" :> 200 :> true  :> "c3",
          "2" :> 20 :> false :> "a2" :> 3.0 :> true  :> "b3" :> 300 :> false :> "c4"
        ],
        expected: [
          "c3", 
          "c4"
        ], 
        execute: \m -> [
          fun m "1" 10 true  "a1" 2.0 false "b2" 200 true, 
          fun m "2" 20 false "a2" 3.0 true  "b3" 300 false
        ],
        executeFailed: Just \m -> [ fun m "2" 20 false "a2" 3.0 true "b3" 300 true ],
        verifyMock: \m -> do 
          verify m $ "1" :> 10 :> true  :> "a1" :> 2.0 :> false :> "b2" :> 200 :> true
          verify m $ "2" :> 20 :> false :> "a2" :> 3.0 :> true  :> "b3" :> 300 :> false
        ,
        verifyCount: \m c -> do
          verifyCount m c $ "1" :> 10 :> true  :> "a1" :> 2.0 :> false :> "b2" :> 200 :> true
          verifyCount m c $ "2" :> 20 :> false :> "a2" :> 3.0 :> true  :> "b3" :> 300 :> false
        ,
        verifyFailed: \m -> verify m $ "1" :> 10 :> true :> "a1" :> 2.0 :> false :> "b2" :> 200 :> false
      }

    describe "Specify the number of Verify times in detail" do
      it "GreaterThanEqual" do
        let 
          m = mock $ "a" :> 10
          _ = fun m "a"
          _ = fun m "a"
          _ = fun m "a"
        verifyCount m (GreaterThanEqual 3) "a"
      it "LessThanEqual" do
        let 
          m = mock $ "a" :> 10
          _ = fun m "a"
          _ = fun m "a"
          _ = fun m "a"
        verifyCount m (LessThanEqual 3) "a"
      it "GreaterThan" do
        let 
          m = mock $ "a" :> 10
          _ = fun m "a"
          _ = fun m "a"
          _ = fun m "a"
        verifyCount m (GreaterThan 2) "a"
      it "LessThan" do
        let 
          m = mock $ "a" :> 10
          _ = fun m "a"
          _ = fun m "a"
          _ = fun m "a"
        verifyCount m (LessThan 4) "a"
    
    describe "Matcher" do
      mockTest {
        name: "Handling Arbitrary Arguments.", 
        create: \_ -> mock $ any :> 11,
        expected: [11, 11, 11], 
        execute: \m -> [fun m "1233", fun m "1234", fun m "2234"],
        executeFailed: Nothing,
        verifyMock: \m -> verify m "1234",
        verifyCount: \m c -> verifyCount m c "1234",
        verifyFailed: \m -> verify m "foo"
      }

      mockTest {
        name: "Verify with arbitrary arguments", 
        create: \_ -> mock $ "1234" :> 11,
        expected: 11, 
        execute: \m -> fun m "1234",
        executeFailed: Just \m -> fun m "1233",
        verifyMock: \m -> verify m (any @String),
        verifyCount: \m c -> verifyCount m c (any @String),
        verifyFailed: \m -> verify m "not called param"
      }

      mockTest {
        name: "Handling arguments with your own Matcher.", 
        create: \_ -> mock $ matcher (_ > 10) "> 10" :> "Expected",
        expected: "Expected", 
        execute: \m -> fun m 11,
        executeFailed: Just \m -> fun m 10,
        verifyMock: \m -> verify m 11,
        verifyCount: \m c -> verifyCount m c 11,
        verifyFailed: \m -> verify m 10
      }

      mockTest {
        name: "Verify arguments with your own Matcher", 
        create: \_ -> mock $ 10 :> "Expected",
        expected: "Expected", 
        execute: \m -> fun m 10,
        executeFailed: Just \m -> fun m 1000,
        verifyMock: \m -> verify m $ matcher (_ < 11) "< 11",
        verifyCount: \m c -> verifyCount m c $ matcher (_ > 9) "> 9",
        verifyFailed: \m -> verify m $ matcher (_ > 11) "> 11"
      }

      mockTest {
        name: "Handling Logical Matcher Or.", 
        create: \_ -> mock $ "a" `or` "b" `or` "c" :> 111,
        expected: [111, 111, 111], 
        execute: \m -> [fun m "a", fun m "b", fun m "c"],
        executeFailed: Just \m -> [fun m "d"],
        verifyMock: \m -> verify m "a",
        verifyCount: \m c -> verifyCount m c "b",
        verifyFailed: \m -> verify m "d"
      }

      mockTest {
        name: "Handling Logical Matcher And.", 
        create: \_ -> mock $ (matcher (_ >= 5) ">= 5") `and` (matcher (_ <= 7) "<= 7") :> true :> 10,
        expected: [10, 10, 10], 
        execute: \m -> [fun m 5 true, fun m 6 true, fun m 7 true],
        executeFailed: Just \m -> [fun m 8 true],
        verifyMock: \m -> verify m $ 5 :> true,
        verifyCount: \m c -> verifyCount m c $ 6 :> true,
        verifyFailed: \m -> verify m $ 8 :> true
      }

      mockTest {
        name: "Include Not Matcher as an argument.",
        create: \_ -> mock $ not "X" :> 11,
        expected: 11,
        execute: \m -> fun m "x",
        executeFailed: Just \m -> fun m "X",
        verifyMock: \m -> verify m "x",
        verifyCount: \m c -> verifyCount m c "x",
        verifyFailed: \m -> verify m "X"
      }

      mockTest {
        name: "Verify with Not Matcher.",
        create: \_ -> mock $ "X" :> 11,
        expected: 11,
        execute: \m -> fun m "X",
        executeFailed: Nothing,
        verifyMock: \m -> verify m $ not "x",
        verifyCount: \m c -> verifyCount m c $ not "x",
        verifyFailed: \m -> verify m "x"
      }

      mockTest {
        name: "Include Not Matcher (function) as an argument.",
        create: \_ -> mock $ (not $ matcher (_ > 10) "> 10") :> 11,
        expected: 11,
        execute: \m -> fun m 10,
        executeFailed: Just \m -> fun m 11,
        verifyMock: \m -> verify m 10,
        verifyCount: \m c -> verifyCount m c 10,
        verifyFailed: \m -> verify m 11
      }

      it "Arbitrary Arguments All Match Arg1" do
        let
          m = mock $ any :> 100

          _ = fun m 30
          _ = fun m 40

        verify m $ MatchAll $ matcher (_ >= 30) ">= 30"

      it "Arbitrary Arguments All Match Arg2" do
        let
          m = mock $ "Title" :> any :> false

          _ = fun m "Title" 2020
          _ = fun m "Title" 2001

        verify m $ MatchAll $ "Title" :> matcher (_ > 2000) "> 2000"

    describe "Order Verification" do
      describe "Verify exactly sequential order." do
        mockOrderTest {
          name: "1 Arguments", 
          create: \_ -> mock $ any :> unit,
          execute: \m -> do
            let
              _ = fun m "a"
              _ = fun m "b"
              _ = fun m "c"
            unit,
          verifyMock: \m -> verifySequence m [
            "a",
            "b",
            "c"
          ],
          verifyFailed: \m -> verifySequence m [
            "a",
            "b",
            "b"
          ]
        }

        mockOrderTest {
          name: "2 Arguments", 
          create: \_ -> mock $ any :> any :> unit,
          execute: \m -> do
            let
              _ = fun m "a" 1
              _ = fun m "b" 2
              _ = fun m "c" 3
            unit,
          verifyMock: \m -> verifySequence m [
            "a" :> 1,
            "b" :> 2,
            "c" :> 3
          ],
          verifyFailed: \m -> verifySequence m [
            "a" :> 2,
            "b" :> 2,
            "c" :> 3
          ]
        }

        mockOrderTest {
          name: "number of function calls doesn't match the number of params", 
          create: \_ -> mock $ any :> unit,
          execute: \m -> do
            let
              _ = fun m "a"
            unit,
          verifyMock: \m -> verifySequence m [
            "a"
          ],
          verifyFailed: \m -> verifySequence m [
            "a",
            "b"
          ]
        }
      
      describe "Verify partially sequential order." do
        mockOrderTest {
          name: "1 Arguments", 
          create: \_ -> mock $ any :> unit,
          execute: \m -> do
            let
              _ = fun m "a"
              _ = fun m "b"
              _ = fun m "c"
            unit,
          verifyMock: \m -> verifyPartiallySequence m [
            "a",
            "c"
          ],
          verifyFailed: \m -> verifyPartiallySequence m [
            "b",
            "a"
          ]
        }

        mockOrderTest {
          name: "2 Arguments", 
          create: \_ -> mock $ any :> any :> unit,
          execute: \m -> do
            let
              _ = fun m "a" true
              _ = fun m "b" false
              _ = fun m "c" true
            unit,
          verifyMock: \m -> verifyPartiallySequence m [
            "a" :> true,
            "c" :> true
          ],
          verifyFailed: \m -> verifyPartiallySequence m [
            "b" :> false,
            "a" :> true
          ]
        }

        mockOrderTest {
          name: "Uncalled value specified.", 
          create: \_ -> mock $ any :> unit,
          execute: \m -> do
            let
              _ = fun m "a"
              _ = fun m "b"
              _ = fun m "c"
            unit,
          verifyMock: \m -> verifyPartiallySequence m [
            "b",
            "c"
          ],
          verifyFailed: \m -> verifyPartiallySequence m [
            "a",
            "d"
          ]
        }

        mockOrderTest {
          name: "number of function calls doesn't match the number of params", 
          create: \_ -> mock $ any :> unit,
          execute: \m -> do
            let
              _ = fun m "a"
            unit,
          verifyMock: \m -> verifyPartiallySequence m [
            "a"
          ],
          verifyFailed: \m -> verifyPartiallySequence m [
            "a",
            "b"
          ]
        }

    describe "Utility" do
      it "Create mock functions directly." do
        let
          fn = mockFun $ "a" :> true :> 300
        fn "a" true `shouldEqual` 300
      mockIt "Supplemental runtime exceptions `it`" \_ -> do
        let
          m = mock $ 1 :> 2
        -- If you change the following values to values different from the expected values, you will see that you have supplemented the exception.
        fun m 1 `shouldEqual` 2

    -- Type annotation is required depending on the monad to be returned.
    describe "Monad" do
      it "Return Monad." do
        let
          m = mock $ "Article Id" :> pure @Aff { title: "Article Title" }

        result <- fun m "Article Id"

        result `shouldEqual` {title: "Article Title"}
        
        verify m "Article Id"
      
      it "Return Monad(update)." do
        let
          updateMock = mock $ "New Title" :> pure @(StateT State Aff) unit
        _ <- runStateT (fun updateMock "New Title") {article: {title: "Old Title"}} 
        verify updateMock "New Title"

    mockTest {
      name: "ADT", 
      create: \_ -> mock $ (Data1 "data1") :> "data1",
      expected: "data1", 
      execute: \m -> fun m (Data1 "data1"),
      executeFailed: Just \m -> fun m (Data1 "data2"),
      verifyMock: \m -> verify m (Data1 "data1"),
      verifyCount: \m c -> verifyCount m c (Data1 "data1"),
      verifyFailed: \m -> verify m (Data2 "data1")
    }

    describe "Cons" do
      describe "Show" do
        it "2 arguments" do
          show (10 :> true) `shouldEqual` "10,true"
        it "3 arguments" do
          show ("1" :> false :> [3, 4]) `shouldEqual` "\"1\",false,[3,4]"
      describe "Eq" do
        it "2 arguments" do
          (1 :> "2") `shouldEqual` (1 :> "2")
        it "3 arguments" do
          ("1" :> false :> [3, 4]) `shouldEqual` ("1" :> false :> [3, 4])

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

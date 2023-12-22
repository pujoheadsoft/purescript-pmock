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
import Test.PMock (VerifyMatchType(..), and, any, fun, hasBeenCalledInOrder, hasBeenCalledInPartialOrder, hasBeenCalledTimes, hasBeenCalledTimesGreaterThan, hasBeenCalledTimesGreaterThanEqual, hasBeenCalledTimesLessThan, hasBeenCalledTimesLessThanEqual, hasBeenCalledWith, hasNotBeenCalledWith, matcher, mock, mockFun, namedMock, notEqual, or, with, (:>))
import Test.PMockSpecs (expectErrorWithMessage, mockIt, runRuntimeThrowableFunction)
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
    describe "Single calls" do

      mockTest {
        name: "1 argument", 
        create: \_ -> mock $ "1" :> 1,
        expected: 1, 
        execute: \m -> fun m "1",
        executeFailed: Just \m -> fun m "2",
        verifyMock: \m -> m `hasBeenCalledWith` "1",
        verifyCount: \m c -> m `hasBeenCalledTimes` c `with` "1",
        verifyFailed: \m -> m `hasBeenCalledWith` "2"
      }

      mockTest {
        name: "2 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true,
        expected: true, 
        execute: \m -> fun m 100 "1",
        executeFailed: Just \m -> fun m 100 "2",
        verifyMock: \m -> m `hasBeenCalledWith` (100 :> "1"),
        verifyCount: \m c -> m `hasBeenCalledTimes` c `with` (100 :> "1"),
        verifyFailed: \m -> m `hasBeenCalledWith` (100 :> "2")
      }

      mockTest {
        name: "3 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1,
        expected: 11.1, 
        execute: \m -> fun m 100 "1" true,
        executeFailed: Just \m -> fun m 100 "1" false,
        verifyMock: \m -> m `hasBeenCalledWith` (100 :> "1" :> true),
        verifyCount: \m c -> m `hasBeenCalledTimes` c `with` (100 :> "1" :> true),
        verifyFailed: \m -> m `hasBeenCalledWith` (100 :> "1" :> false)
      }

      mockTest {
        name: "4 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2],
        expected: [1, 2], 
        execute: \m -> fun m 100 "1" true 11.1,
        executeFailed: Just \m -> fun m 100 "1" true 11.0,
        verifyMock: \m -> m `hasBeenCalledWith` (100 :> "1" :> true :> 11.1),
        verifyCount: \m c -> m `hasBeenCalledTimes` c `with` (100 :> "1" :> true :> 11.1),
        verifyFailed: \m -> m `hasBeenCalledWith` (100 :> "1" :> true :> 11.0)
      }

      mockTest {
        name: "5 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"},
        expected: {name: "Name"}, 
        execute: \m -> fun m 100 "1" true 11.1 [1, 2],
        executeFailed: Just \m -> fun m 100 "1" true 11.1 [1, 3],
        verifyMock: \m -> m `hasBeenCalledWith` (100 :> "1" :> true :> 11.1 :> [1, 2]),
        verifyCount: \m c -> m `hasBeenCalledTimes` c `with` (100 :> "1" :> true :> 11.1 :> [1, 2]),
        verifyFailed: \m -> m `hasBeenCalledWith` (100 :> "1" :> true :> 11.1 :> [2, 2])
      }

      mockTest {
        name: "6 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20,
        expected: 20, 
        execute: \m -> fun m 100 "1" true 11.1 [1, 2] {name: "Name"},
        executeFailed: Just \m -> fun m 100 "1" true 11.1 [1, 3] {name: "Nam"},
        verifyMock: \m -> m `hasBeenCalledWith` (100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"}),
        verifyCount: \m c -> m `hasBeenCalledTimes` c `with` (100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"}),
        verifyFailed: \m -> m `hasBeenCalledWith` (100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Nome"})
      }

      mockTest {
        name: "7 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X",
        expected: "X", 
        execute: \m -> fun m 100 "1" true 11.1 [1, 2] {name: "Name"} 20,
        executeFailed: Just \m -> fun m 100 "1" true 11.1 [1, 3] {name: "Name"} 21,
        verifyMock: \m -> m `hasBeenCalledWith` (100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20),
        verifyCount: \m c -> m `hasBeenCalledTimes` c `with` (100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20),
        verifyFailed: \m -> m `hasBeenCalledWith` (100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 19)
      }

      mockTest {
        name: "8 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> false,
        expected: false, 
        execute: \m -> fun m 100 "1" true 11.1 [1, 2] {name: "Name"} 20 "X",
        executeFailed: Just \m -> fun m 100 "1" true 11.1 [1, 3] {name: "Name"} 20 "Y",
        verifyMock: \m -> m `hasBeenCalledWith` (100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X"),
        verifyCount: \m c -> m `hasBeenCalledTimes` c `with` (100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X"),
        verifyFailed: \m -> m `hasBeenCalledWith` (100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "Z")
      }

      mockTest {
        name: "9 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> false :> 0.1,
        expected: 0.1, 
        execute: \m -> fun m 100 "1" true 11.1 [1, 2] {name: "Name"} 20 "X" false,
        executeFailed: Just \m -> fun m 100 "1" true 11.1 [1, 3] {name: "Name"} 20 "X" true,
        verifyMock: \m -> m `hasBeenCalledWith` (100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> false),
        verifyCount: \m c -> m `hasBeenCalledTimes` c `with` (100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> false),
        verifyFailed: \m -> m `hasBeenCalledWith` (100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> true)
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
          m `hasBeenCalledWith` "1"
          m `hasBeenCalledWith` "2"
        ,
        verifyCount: \m c -> do
          m `hasBeenCalledTimes` c `with` "1"
          m `hasBeenCalledTimes` c `with` "2"
        ,
        verifyFailed: \m -> m `hasBeenCalledWith` "3"
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
          m `hasBeenCalledWith` ("1" :> 10)
          m `hasBeenCalledWith` ("2" :> 20)
        ,
        verifyCount: \m c -> do
          m `hasBeenCalledTimes` c `with` ("1" :> 10)
          m `hasBeenCalledTimes` c `with` ("2" :> 20)
        ,
        verifyFailed: \m -> m `hasBeenCalledWith` ("1" :> 30)
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
          m `hasBeenCalledWith` ("1" :> 10 :> true)
          m `hasBeenCalledWith` ("2" :> 20 :> false)
        ,
        verifyCount: \m c -> do
          m `hasBeenCalledTimes` c `with` ("1" :> 10 :> true)
          m `hasBeenCalledTimes` c `with` ("2" :> 20 :> false)
        ,
        verifyFailed: \m -> m `hasBeenCalledWith` ("1" :> 10 :> false)
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
          m `hasBeenCalledWith` ("1" :> 10 :> true  :> "a1")
          m `hasBeenCalledWith` ("2" :> 20 :> false :> "a2")
        ,
        verifyCount: \m c -> do
          m `hasBeenCalledTimes` c `with` ("1" :> 10 :> true  :> "a1")
          m `hasBeenCalledTimes` c `with` ("2" :> 20 :> false :> "a2")
        ,
        verifyFailed: \m -> m `hasBeenCalledWith` ("1" :> 10 :> true :> "a3")
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
          m `hasBeenCalledWith` ("1" :> 10 :> true  :> "a1" :> 2.0)
          m `hasBeenCalledWith` ("2" :> 20 :> false :> "a2" :> 3.0)
        ,
        verifyCount: \m c -> do
          m `hasBeenCalledTimes` c `with` ("1" :> 10 :> true  :> "a1" :> 2.0)
          m `hasBeenCalledTimes` c `with` ("2" :> 20 :> false :> "a2" :> 3.0)
        ,
        verifyFailed: \m -> m `hasBeenCalledWith` ("1" :> 10 :> true :> "a1" :> 1.0)
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
          m `hasBeenCalledWith` ("1" :> 10 :> true  :> "a1" :> 2.0 :> false)
          m `hasBeenCalledWith` ("2" :> 20 :> false :> "a2" :> 3.0 :> true)
        ,
        verifyCount: \m c -> do
          m `hasBeenCalledTimes` c `with` ("1" :> 10 :> true  :> "a1" :> 2.0 :> false)
          m `hasBeenCalledTimes` c `with` ("2" :> 20 :> false :> "a2" :> 3.0 :> true)
        ,
        verifyFailed: \m -> m `hasBeenCalledWith` ("1" :> 10 :> true :> "a1" :> 2.0 :> true)
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
          m `hasBeenCalledWith` ("1" :> 10 :> true  :> "a1" :> 2.0 :> false :> "b2")
          m `hasBeenCalledWith` ("2" :> 20 :> false :> "a2" :> 3.0 :> true  :> "b3")
        ,
        verifyCount: \m c -> do
          m `hasBeenCalledTimes` c `with` ("1" :> 10 :> true  :> "a1" :> 2.0 :> false :> "b2")
          m `hasBeenCalledTimes` c `with` ("2" :> 20 :> false :> "a2" :> 3.0 :> true  :> "b3")
        ,
        verifyFailed: \m -> m `hasBeenCalledWith` ("1" :> 10 :> true :> "a1" :> 2.0 :> false :> "b3")
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
          m `hasBeenCalledWith` ("1" :> 10 :> true  :> "a1" :> 2.0 :> false :> "b2" :> 200)
          m `hasBeenCalledWith` ("2" :> 20 :> false :> "a2" :> 3.0 :> true  :> "b3" :> 300)
        ,
        verifyCount: \m c -> do
          m `hasBeenCalledTimes` c `with` ("1" :> 10 :> true  :> "a1" :> 2.0 :> false :> "b2" :> 200)
          m `hasBeenCalledTimes` c `with` ("2" :> 20 :> false :> "a2" :> 3.0 :> true  :> "b3" :> 300)
        ,
        verifyFailed: \m -> m `hasBeenCalledWith` ("1" :> 10 :> true :> "a1" :> 2.0 :> false :> "b2" :> 300)
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
          m `hasBeenCalledWith` ("1" :> 10 :> true  :> "a1" :> 2.0 :> false :> "b2" :> 200 :> true)
          m `hasBeenCalledWith` ("2" :> 20 :> false :> "a2" :> 3.0 :> true  :> "b3" :> 300 :> false)
        ,
        verifyCount: \m c -> do
          m `hasBeenCalledTimes` c `with` ("1" :> 10 :> true  :> "a1" :> 2.0 :> false :> "b2" :> 200 :> true)
          m `hasBeenCalledTimes` c `with` ("2" :> 20 :> false :> "a2" :> 3.0 :> true  :> "b3" :> 300 :> false)
        ,
        verifyFailed: \m -> m `hasBeenCalledWith` ("1" :> 10 :> true :> "a1" :> 2.0 :> false :> "b2" :> 200 :> false)
      }

    describe "Specify the number of times in detail" do
      it "GreaterThanEqual" do
        m <- mock $ "a" :> 10
        let 
          _ = fun m "a"
          _ = fun m "a"
          _ = fun m "a"
        m `hasBeenCalledTimesGreaterThanEqual` 3 `with` "a"
      it "LessThanEqual" do
        m <- mock $ "a" :> 10
        let 
          _ = fun m "a"
          _ = fun m "a"
          _ = fun m "a"
        m `hasBeenCalledTimesLessThanEqual` 3 `with` "a"
      it "GreaterThan" do
        m <- mock $ "a" :> 10
        let   
          _ = fun m "a"
          _ = fun m "a"
          _ = fun m "a"
        m `hasBeenCalledTimesGreaterThan` 2 `with` "a"
      it "LessThan" do
        m <- mock $ "a" :> 10
        let 
          _ = fun m "a"
          _ = fun m "a"
          _ = fun m "a"
        m `hasBeenCalledTimesLessThan` 4 `with` "a"

    describe "has not been called" do
      it "simple mock" do
        m <- mock $ "a" :> 10
        let
          _ = fun m "a"
        m `hasNotBeenCalledWith` "b"

      it "any matcher" do
        m <- mock $ "a" :> 10
        m `hasNotBeenCalledWith` any@String

      it "failed" do
        m <- mock $ "a" :> 10
        let
          _ = fun m "a"
        expectError $ hasNotBeenCalledWith m "a"

      it "multiple mock" do
        m <- mock [
          "a" :> 10,
          "b" :> 20
        ]
        let
          _ = fun m "a"
          _ = fun m "b"
        m `hasNotBeenCalledWith` "c"

    describe "Matcher" do
      mockTest {
        name: "Handling Arbitrary Arguments.", 
        create: \_ -> mock $ any :> 11,
        expected: [11, 11, 11], 
        execute: \m -> [fun m "1233", fun m "1234", fun m "2234"],
        executeFailed: Nothing,
        verifyMock: \m -> m `hasBeenCalledWith` "1234",
        verifyCount: \m c -> m `hasBeenCalledTimes` c $ "1234",
        verifyFailed: \m -> m `hasBeenCalledWith` "foo"
      }

      mockTest {
        name: "with arbitrary arguments", 
        create: \_ -> mock $ "1234" :> 11,
        expected: 11, 
        execute: \m -> fun m "1234",
        executeFailed: Just \m -> fun m "1233",
        verifyMock: \m -> m `hasBeenCalledWith` (any @String),
        verifyCount: \m c -> m `hasBeenCalledTimes` c $ (any @String),
        verifyFailed: \m -> m `hasBeenCalledWith` "not called param"
      }

      mockTest {
        name: "Handling arguments with your own Matcher.", 
        create: \_ -> mock $ matcher (_ > 10) "> 10" :> "Expected",
        expected: "Expected", 
        execute: \m -> fun m 11,
        executeFailed: Just \m -> fun m 10,
        verifyMock: \m -> m `hasBeenCalledWith` 11,
        verifyCount: \m c -> m `hasBeenCalledTimes` c $ 11,
        verifyFailed: \m -> m `hasBeenCalledWith` 10
      }

      mockTest {
        name: "arguments with your own Matcher", 
        create: \_ -> mock $ 10 :> "Expected",
        expected: "Expected", 
        execute: \m -> fun m 10,
        executeFailed: Just \m -> fun m 1000,
        verifyMock: \m -> m `hasBeenCalledWith` matcher (_ < 11) "< 11",
        verifyCount: \m c -> m `hasBeenCalledTimes` c $ matcher (_ > 9) "> 9",
        verifyFailed: \m -> m `hasBeenCalledWith` matcher (_ > 11) "> 11"
      }

      mockTest {
        name: "Handling Logical Matcher Or.", 
        create: \_ -> mock $ "a" `or` "b" `or` "c" :> 111,
        expected: [111, 111, 111], 
        execute: \m -> [fun m "a", fun m "b", fun m "c"],
        executeFailed: Just \m -> [fun m "d"],
        verifyMock: \m -> m `hasBeenCalledWith` "a",
        verifyCount: \m c -> m `hasBeenCalledTimes` c $ "b",
        verifyFailed: \m -> m `hasBeenCalledWith` "d"
      }

      mockTest {
        name: "Handling Logical Matcher And.", 
        create: \_ -> mock $ (matcher (_ >= 5) ">= 5") `and` (matcher (_ <= 7) "<= 7") :> true :> 10,
        expected: [10, 10, 10], 
        execute: \m -> [fun m 5 true, fun m 6 true, fun m 7 true],
        executeFailed: Just \m -> [fun m 8 true],
        verifyMock: \m -> m `hasBeenCalledWith` (5 :> true),
        verifyCount: \m c -> m `hasBeenCalledTimes` c $ 6 :> true,
        verifyFailed: \m -> m `hasBeenCalledWith` (8 :> true)
      }

      mockTest {
        name: "Include Not Matcher as an argument.",
        create: \_ -> mock $ notEqual "X" :> 11,
        expected: 11,
        execute: \m -> fun m "x",
        executeFailed: Just \m -> fun m "X",
        verifyMock: \m -> m `hasBeenCalledWith` "x",
        verifyCount: \m c -> m `hasBeenCalledTimes` c $ "x",
        verifyFailed: \m -> m `hasBeenCalledWith` "X"
      }

      mockTest {
        name: "with Not Matcher.",
        create: \_ -> mock $ "X" :> 11,
        expected: 11,
        execute: \m -> fun m "X",
        executeFailed: Nothing,
        verifyMock: \m -> m `hasBeenCalledWith` notEqual "x",
        verifyCount: \m c -> m `hasBeenCalledTimes` c $ notEqual "x",
        verifyFailed: \m -> m `hasBeenCalledWith` "x"
      }

      mockTest {
        name: "Include Not Matcher (function) as an argument.",
        create: \_ -> mock $ (notEqual $ matcher (_ > 10) "> 10") :> 11,
        expected: 11,
        execute: \m -> fun m 10,
        executeFailed: Just \m -> fun m 11,
        verifyMock: \m -> m `hasBeenCalledWith` 10,
        verifyCount: \m c -> m `hasBeenCalledTimes` c $ 10,
        verifyFailed: \m -> m `hasBeenCalledWith` 11
      }

      it "Arbitrary Arguments All Match Arg1" do
        m <- mock $ any :> 100
        let
          _ = fun m 30
          _ = fun m 40

        m `hasBeenCalledWith` (MatchAll $ matcher (_ >= 30) ">= 30")

      it "Arbitrary Arguments All Match Arg2" do
        m <- mock $ "Title" :> any :> false
        let
          _ = fun m "Title" 2020
          _ = fun m "Title" 2001

        m `hasBeenCalledWith` (MatchAll $ "Title" :> matcher (_ > 2000) "> 2000")

    describe "Order Verification" do
      describe "exactly sequential order." do
        mockOrderTest {
          name: "1 Arguments", 
          create: \_ -> mock $ any :> unit,
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
          create: \_ -> mock $ any :> any :> unit,
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
          create: \_ -> mock $ any :> unit,
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
          create: \_ -> mock $ any :> unit,
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
          create: \_ -> mock $ any :> any :> unit,
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
          create: \_ -> mock $ any :> unit,
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
          create: \_ -> mock $ any :> unit,
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
        m <- mock $ 1 :> 2
        -- If you change the following values to values different from the expected values, you will see that you have supplemented the exception.
        fun m 1 `shouldEqual` 2

    -- Type annotation is required depending on the monad to be returned.
    describe "Monad" do
      it "Return Monad." do
        m <- mock $ "Article Id" :> pure @Aff { title: "Article Title" }

        result <- fun m "Article Id"

        result `shouldEqual` {title: "Article Title"}
        
        m `hasBeenCalledWith` "Article Id"
      
      it "Return Monad(update)." do
        updateMock <- mock $ "New Title" :> pure @(StateT State Aff) unit
        _ <- runStateT (fun updateMock "New Title") {article: {title: "Old Title"}} 
        updateMock `hasBeenCalledWith` "New Title"

    mockTest {
      name: "ADT", 
      create: \_ -> mock $ (Data1 "data1") :> "data1",
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
          m <- mock $ "a" :> 100
          let
            expected = joinWith "\n" [
              "Error: function was not called with expected arguments.",
              "  expected: \"a\"",
              "  but was : \"b\""
            ]
          expectErrorWithMessage expected $ runRuntimeThrowableFunction \_ -> fun m "b"

        it "multi mock" do
          m <- mock [
            "aaa" :> 100 :> true,
            "bbb" :> 200 :> false
          ]
          let
            expected = joinWith "\n" [
              "Error: function was not called with expected arguments.",
              "  expected one of the following:",
              "    \"aaa\",100",
              "    \"bbb\",200",
              "  but was actual:",
              "    \"aaa\",200"
            ]
          expectErrorWithMessage expected $ runRuntimeThrowableFunction \_ -> fun m "aaa" 200

      describe "verify" do
        it "simple mock verify" do
          m <- mock $ any@String :> 100
          let
            _ = fun m "A"
            expected = joinWith "\n" [
              "function was not called with expected arguments.",
              "  expected: \"X\"",
              "  but was : \"A\""
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledWith` "X"

        it "count" do
          m <- mock $ any@String :> 100
          let
            _ = fun m "A"
            expected = joinWith "\n" [
              "function was not called the expected number of times.",
              "  expected: 2",
              "  but was : 1"
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledTimes` 2 `with` "A"

        it "verifySequence" do
          m <- mock $ any@String :> 100
          let
            _ = fun m "B"
            _ = fun m "C"
            _ = fun m "A"
            expected = joinWith "\n" [
              "function was not called with expected order.",
              "  expected 1st call: \"A\"",
              "  but was  1st call: \"B\"",
              "  expected 2nd call: \"B\"",
              "  but was  2nd call: \"C\"",
              "  expected 3rd call: \"C\"",
              "  but was  3rd call: \"A\""
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledInOrder` ["A", "B", "C"]
        
        it "verifySequence (count mismatch)" do
          m <- mock $ any@String :> 100
          let
            _ = fun m "B"
            _ = fun m "C"
            expected = joinWith "\n" [
              "The number of function calls doesn't match the number of params.",
              "  number of function calls: 2",
              "  number of params:         3"
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledInOrder` ["A", "B", "C"]
        
        it "verifyPartiallySequence" do
          m <- mock $ any@String :> 100
          let
            _ = fun m "B"
            _ = fun m "A"
            expected = joinWith "\n" [
              "function was not called with expected order.",
              "  expected order:",
              "    \"A\"",
              "    \"C\"",
              "  actual order:",
              "    \"B\"",
              "    \"A\""
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledInPartialOrder` ["A", "C"]

        it "verifyPartiallySequence (count mismatch)" do
          m <- mock $ any@String :> 100
          let
            _ = fun m "B"
            expected = joinWith "\n" [
              "The number of parameters exceeds the number of function calls.",
              "  number of function calls: 1",
              "  number of params:         2"
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledInPartialOrder` ["A", "C"]

    describe "named mock" do
      describe "call" do
        it "simple mock"  do
          m <- namedMock "mock function" $ "a" :> 100
          let
            expected = joinWith "\n" [
              "Error: function `mock function` was not called with expected arguments.",
              "  expected: \"a\"",
              "  but was : \"b\""
            ]
          expectErrorWithMessage expected $ runRuntimeThrowableFunction \_ -> fun m "b"

        it "multi mock" do
          m <- namedMock "mock function" [
            "aaa" :> 100 :> true,
            "bbb" :> 200 :> false
          ]
          let
            expected = joinWith "\n" [
              "Error: function `mock function` was not called with expected arguments.",
              "  expected one of the following:",
              "    \"aaa\",100",
              "    \"bbb\",200",
              "  but was actual:",
              "    \"aaa\",200"
            ]
          expectErrorWithMessage expected $ runRuntimeThrowableFunction \_ -> fun m "aaa" 200

      describe "verify" do
        it "simple mock verify" do
          m <- namedMock "mock function" $ any@String :> 100
          let
            _ = fun m "A"
            expected = joinWith "\n" [
              "function `mock function` was not called with expected arguments.",
              "  expected: \"X\"",
              "  but was : \"A\""
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledWith` "X"

        it "count" do
          m <- namedMock "mock function" $ any@String :> 100
          let
            _ = fun m "A"
            expected = joinWith "\n" [
              "function `mock function` was not called the expected number of times.",
              "  expected: 2",
              "  but was : 1"
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledTimes` 2 `with` "A"

        it "verifySequence" do
          m <- namedMock "mock function" $ any@String :> 100
          let
            _ = fun m "B"
            _ = fun m "C"
            _ = fun m "A"
            expected = joinWith "\n" [
              "function `mock function` was not called with expected order.",
              "  expected 1st call: \"A\"",
              "  but was  1st call: \"B\"",
              "  expected 2nd call: \"B\"",
              "  but was  2nd call: \"C\"",
              "  expected 3rd call: \"C\"",
              "  but was  3rd call: \"A\""
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledInOrder` ["A", "B", "C"]
        
        it "verifySequence (count mismatch)" do
          m <- namedMock "mockFunc" $ any@String :> 100
          let
            _ = fun m "B"
            _ = fun m "C"
            expected = joinWith "\n" [
              "The number of function `mockFunc` calls doesn't match the number of params.",
              "  number of function calls: 2",
              "  number of params:         3"
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledInOrder` ["A", "B", "C"]
        
        it "verifyPartiallySequence" do
          m <- namedMock "mock function" $ any@String :> 100
          let
            _ = fun m "B"
            _ = fun m "A"
            expected = joinWith "\n" [
              "function `mock function` was not called with expected order.",
              "  expected order:",
              "    \"A\"",
              "    \"C\"",
              "  actual order:",
              "    \"B\"",
              "    \"A\""
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledInPartialOrder` ["A", "C"]

        it "verifyPartiallySequence (count mismatch)" do
          m <- namedMock "mockFunc" $ any@String :> 100
          let
            _ = fun m "B"
            expected = joinWith "\n" [
              "The number of parameters exceeds the number of function `mockFunc` calls.",
              "  number of function calls: 1",
              "  number of params:         2"
            ]
          expectErrorWithMessage expected $ m `hasBeenCalledInPartialOrder` ["A", "C"]

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

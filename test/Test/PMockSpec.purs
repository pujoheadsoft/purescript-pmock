module Test.PMockSpec where

import Prelude

import Control.Monad.Except (class MonadError)
import Control.Monad.State (StateT, runStateT)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Error)
import Test.PMock (CountVerifyMethod(..), Param, any, matcher, mock, run, runRuntimeThrowableFunction, verify, verifyCount, (:>))
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

spec :: Spec Unit
spec = do
  describe "PMock Test" do
    describe "Single calles" do

      mockTest {
        name: "1 argument", 
        create: \_ -> mock $ "1" :> 1,
        expected: 1, 
        execute: \m -> run m "1",
        executeFailed: Just \m -> run m "2",
        verifyMock: \m -> verify m "1",
        verifyCount: \m c -> verifyCount m c "1",
        verifyFailed: \m -> verify m "2"
      }

      mockTest {
        name: "2 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true,
        expected: true, 
        execute: \m -> run m 100 "1",
        executeFailed: Just \m -> run m 100 "2",
        verifyMock: \m -> verify m $ 100 :> "1",
        verifyCount: \m c -> verifyCount m c $ 100 :> "1",
        verifyFailed: \m -> verify m $ 100 :> "2"
      }

      mockTest {
        name: "3 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1,
        expected: 11.1, 
        execute: \m -> run m 100 "1" true,
        executeFailed: Just \m -> run m 100 "1" false,
        verifyMock: \m -> verify m $ 100 :> "1" :> true,
        verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true,
        verifyFailed: \m -> verify m $ 100 :> "1" :> false
      }

      mockTest {
        name: "4 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2],
        expected: [1, 2], 
        execute: \m -> run m 100 "1" true 11.1,
        executeFailed: Just \m -> run m 100 "1" true 11.0,
        verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1,
        verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1,
        verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.0
      }

      mockTest {
        name: "5 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"},
        expected: {name: "Name"}, 
        execute: \m -> run m 100 "1" true 11.1 [1, 2],
        executeFailed: Just \m -> run m 100 "1" true 11.1 [1, 3],
        verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2],
        verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1 :> [1, 2],
        verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [2, 2]
      }

      mockTest {
        name: "6 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20,
        expected: 20, 
        execute: \m -> run m 100 "1" true 11.1 [1, 2] {name: "Name"},
        executeFailed: Just \m -> run m 100 "1" true 11.1 [1, 3] {name: "Nam"},
        verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"},
        verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"},
        verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Nome"}
      }

      mockTest {
        name: "7 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X",
        expected: "X", 
        execute: \m -> run m 100 "1" true 11.1 [1, 2] {name: "Name"} 20,
        executeFailed: Just \m -> run m 100 "1" true 11.1 [1, 3] {name: "Name"} 21,
        verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20,
        verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20,
        verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 19
      }

      mockTest {
        name: "8 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> false,
        expected: false, 
        execute: \m -> run m 100 "1" true 11.1 [1, 2] {name: "Name"} 20 "X",
        executeFailed: Just \m -> run m 100 "1" true 11.1 [1, 3] {name: "Name"} 20 "Y",
        verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X",
        verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X",
        verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "Z"
      }

      mockTest {
        name: "9 arguments", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> false :> 0.1,
        expected: 0.1, 
        execute: \m -> run m 100 "1" true 11.1 [1, 2] {name: "Name"} 20 "X" false,
        executeFailed: Just \m -> run m 100 "1" true 11.1 [1, 3] {name: "Name"} 20 "X" true,
        verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> false,
        verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> false,
        verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> true
      }

    describe "Multiple calles" do
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
          run m "1", run m "2"
        ],
        executeFailed: Just \m -> [ run m "3" ],
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
          run m "1" 10, 
          run m "2" 20
        ],
        executeFailed: Just \m -> [ run m "2" 10 ],
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
          run m "1" 10 true, 
          run m "2" 20 false
        ],
        executeFailed: Just \m -> [ run m "2" 20 true ],
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
          run m "1" 10 true  "a1", 
          run m "2" 20 false "a2"
        ],
        executeFailed: Just \m -> [ run m "2" 20 false "a1" ],
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
          run m "1" 10 true  "a1" 2.0, 
          run m "2" 20 false "a2" 3.0
        ],
        executeFailed: Just \m -> [ run m "2" 20 false "a2" 3.1 ],
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
          run m "1" 10 true  "a1" 2.0 false, 
          run m "2" 20 false "a2" 3.0 true
        ],
        executeFailed: Just \m -> [ run m "2" 20 false "a2" 3.0 false ],
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
          run m "1" 10 true  "a1" 2.0 false "b2", 
          run m "2" 20 false "a2" 3.0 true  "b3"
        ],
        executeFailed: Just \m -> [ run m "2" 20 false "a2" 3.0 true "b2" ],
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
          run m "1" 10 true  "a1" 2.0 false "b2" 200, 
          run m "2" 20 false "a2" 3.0 true  "b3" 300
        ],
        executeFailed: Just \m -> [ run m "2" 20 false "a2" 3.0 true "b3" 200 ],
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
          run m "1" 10 true  "a1" 2.0 false "b2" 200 true, 
          run m "2" 20 false "a2" 3.0 true  "b3" 300 false
        ],
        executeFailed: Just \m -> [ run m "2" 20 false "a2" 3.0 true "b3" 300 true ],
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
          _ = run m "a"
          _ = run m "a"
          _ = run m "a"
        verifyCount m (GreaterThanEqual 3) "a"
      it "LessThanEqual" do
        let 
          m = mock $ "a" :> 10
          _ = run m "a"
          _ = run m "a"
          _ = run m "a"
        verifyCount m (LessThanEqual 3) "a"
      it "GreaterThan" do
        let 
          m = mock $ "a" :> 10
          _ = run m "a"
          _ = run m "a"
          _ = run m "a"
        verifyCount m (GreaterThan 2) "a"
      it "LessThan" do
        let 
          m = mock $ "a" :> 10
          _ = run m "a"
          _ = run m "a"
          _ = run m "a"
        verifyCount m (LessThan 4) "a"

    describe "Matcher" do
      mockTest {
        name: "Handling Arbitrary Arguments.", 
        create: \_ -> mock $ (any :: Param String) :> 11,
        expected: [11, 11, 11], 
        execute: \m -> [run m "1233", run m "1234", run m "2234"],
        executeFailed: Nothing,
        verifyMock: \m -> verify m "1234",
        verifyCount: \m c -> verifyCount m c "1234",
        verifyFailed: \m -> verify m "foo"
      }

      mockTest {
        name: "Verify with arbitrary arguments", 
        create: \_ -> mock $ "1234" :> 11,
        expected: 11, 
        execute: \m -> run m "1234",
        executeFailed: Just \m -> run m "1233",
        verifyMock: \m -> verify m (any :: Param String),
        verifyCount: \m c -> verifyCount m c (any :: Param String),
        verifyFailed: \m -> verify m "not called param"
      }

      mockTest {
        name: "Handling arguments with your own Matcher.", 
        create: \_ -> mock $ matcher (\v -> v > 10) "> 10" :> "Expected",
        expected: "Expected", 
        execute: \m -> run m 11,
        executeFailed: Just \m -> run m 10,
        verifyMock: \m -> verify m 11,
        verifyCount: \m c -> verifyCount m c 11,
        verifyFailed: \m -> verify m 10
      }

      mockTest {
        name: "Verify arguments with your own Matcher", 
        create: \_ -> mock $ 10 :> "Expected",
        expected: "Expected", 
        execute: \m -> run m 10,
        executeFailed: Just \m -> run m 1000,
        verifyMock: \m -> verify m $ matcher (\v -> v < 11) "< 11",
        verifyCount: \m c -> verifyCount m c $ matcher (\v -> v > 9) "> 9",
        verifyFailed: \m -> verify m $ matcher (\v -> v > 11) "> 11"
      }

    -- Type annotation is required depending on the monad to be returned.
    describe "Monad" do
      it "Return Monad." do
        let
          m = mock $ "Article Id" :> (pure { title: "Article Title" } :: Aff Article)

        result <- run m "Article Id"

        result `shouldEqual` {title: "Article Title"}
        
        verify m "Article Id"
      
      it "Return Monad(update)." do
        let
          updateMock = mock $ "New Title" :> (pure unit :: StateT State Aff Unit)
        _ <- runStateT (run updateMock "New Title") {article: {title: "Old Title"}} 
        verify updateMock "New Title"

    describe "Cons" do
      describe "Show" do
        it "2 arguments" do
          show (10 :> true) `shouldEqual` "10, true"
        it "3 arguments" do
          show ("1" :> false :> [3, 4]) `shouldEqual` "\"1\", false, [3,4]"
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
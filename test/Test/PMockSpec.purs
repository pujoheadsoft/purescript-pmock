module Test.PMockSpec where

import Prelude

import Control.Monad.Except (class MonadError)
import Control.Monad.State (StateT, runStateT)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Error)
import Test.PMock (CountVerifyMethod(..), Param, any, matcher, mock, runRuntimeThrowableFunction, verify, verifyCount, (:>))
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
  it "設定した引数で実行すると設定した値を返すことができる" do
    let m = f.create unit
    f.execute m `shouldEqual` f.expected

  it "設定した引数で実行しないと失敗する" do
    case f.executeFailed of
      Just func -> let m = f.create unit
        in expectError $ runRuntimeThrowableFunction (\_ -> func m)
      Nothing -> pure unit

  it "指定した引数で呼び出されたかどうかを検証できる" do
    let 
      m = f.create unit
      _ = f.execute m
    f.verifyMock m

  it "指定した引数で呼び出されていない場合は検証に失敗する" do
    let 
      m = f.create unit
      _ = f.execute m
    expectError $ f.verifyFailed m

  it "指定した引数で呼び出された回数を検証できる(0回)" do
    let m = f.create unit
    f.verifyCount m 0

  it "指定した引数で呼び出された回数を検証できる(複数回)" do
    let 
      m = f.create unit
      _ = f.execute m
      _ = f.execute m
      _ = f.execute m
    f.verifyCount m 3

spec :: Spec Unit
spec = do
  describe "PMockのテスト" do
    describe "Single Mock" do

      mockTest {
        name: "引数が1つの場合", 
        create: \_ -> mock $ "1" :> 1,
        expected: 1, 
        execute: \m -> m.fun "1",
        executeFailed: Just \m -> m.fun "2",
        verifyMock: \m -> verify m "1",
        verifyCount: \m c -> verifyCount m c "1",
        verifyFailed: \m -> verify m "2"
      }

      mockTest {
        name: "引数が2つの場合", 
        create: \_ -> mock $ 100 :> "1" :> true,
        expected: true, 
        execute: \m -> m.fun 100 "1",
        executeFailed: Just \m -> m.fun 100 "2",
        verifyMock: \m -> verify m $ 100 :> "1",
        verifyCount: \m c -> verifyCount m c $ 100 :> "1",
        verifyFailed: \m -> verify m $ 100 :> "2"
      }

      mockTest {
        name: "引数が3つの場合", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1,
        expected: 11.1, 
        execute: \m -> m.fun 100 "1" true,
        executeFailed: Just \m -> m.fun 100 "1" false,
        verifyMock: \m -> verify m $ 100 :> "1" :> true,
        verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true,
        verifyFailed: \m -> verify m $ 100 :> "1" :> false
      }

      mockTest {
        name: "引数が4つの場合", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2],
        expected: [1, 2], 
        execute: \m -> m.fun 100 "1" true 11.1,
        executeFailed: Just \m -> m.fun 100 "1" true 11.0,
        verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1,
        verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1,
        verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.0
      }

      mockTest {
        name: "引数が5つの場合", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"},
        expected: {name: "Name"}, 
        execute: \m -> m.fun 100 "1" true 11.1 [1, 2],
        executeFailed: Just \m -> m.fun 100 "1" true 11.1 [1, 3],
        verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2],
        verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1 :> [1, 2],
        verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [2, 2]
      }

      mockTest {
        name: "引数が6つの場合", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20,
        expected: 20, 
        execute: \m -> m.fun 100 "1" true 11.1 [1, 2] {name: "Name"},
        executeFailed: Just \m -> m.fun 100 "1" true 11.1 [1, 3] {name: "Nam"},
        verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"},
        verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"},
        verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Nome"}
      }

      mockTest {
        name: "引数が7つの場合", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X",
        expected: "X", 
        execute: \m -> m.fun 100 "1" true 11.1 [1, 2] {name: "Name"} 20,
        executeFailed: Just \m -> m.fun 100 "1" true 11.1 [1, 3] {name: "Name"} 21,
        verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20,
        verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20,
        verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 19
      }

      mockTest {
        name: "引数が8つの場合", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> false,
        expected: false, 
        execute: \m -> m.fun 100 "1" true 11.1 [1, 2] {name: "Name"} 20 "X",
        executeFailed: Just \m -> m.fun 100 "1" true 11.1 [1, 3] {name: "Name"} 20 "Y",
        verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X",
        verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X",
        verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "Z"
      }

      mockTest {
        name: "引数が9つの場合", 
        create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> false :> 0.1,
        expected: 0.1, 
        execute: \m -> m.fun 100 "1" true 11.1 [1, 2] {name: "Name"} 20 "X" false,
        executeFailed: Just \m -> m.fun 100 "1" true 11.1 [1, 3] {name: "Name"} 20 "X" true,
        verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> false,
        verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> false,
        verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> true
      }

    describe "Multi Mock" do
      mockTest {
        name: "引数が1つの場合", 
        create: \_ -> mock $ [
          "1" :> 10, 
          "2" :> 20
        ],
        expected: [
          10, 
          20
        ], 
        execute: \m -> [
          m.fun "1", m.fun "2"
        ],
        executeFailed: Just \m -> [ m.fun "3" ],
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
        name: "引数が2つの場合", 
        create: \_ -> mock $ [
          "1" :> 10 :> true, 
          "2" :> 20 :> false
        ],
        expected: [
          true, 
          false
        ], 
        execute: \m -> [
          m.fun "1" 10, 
          m.fun "2" 20
        ],
        executeFailed: Just \m -> [ m.fun "2" 10 ],
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
        name: "引数が3つの場合", 
        create: \_ -> mock $ [
          "1" :> 10 :> true  :> "a1", 
          "2" :> 20 :> false :> "a2"
        ],
        expected: [
          "a1", 
          "a2"
        ], 
        execute: \m -> [
          m.fun "1" 10 true, 
          m.fun "2" 20 false
        ],
        executeFailed: Just \m -> [ m.fun "2" 20 true ],
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
        name: "引数が4つの場合", 
        create: \_ -> mock $ [
          "1" :> 10 :> true  :> "a1" :> 2.0, 
          "2" :> 20 :> false :> "a2" :> 3.0
        ],
        expected: [
          2.0, 
          3.0
        ], 
        execute: \m -> [
          m.fun "1" 10 true  "a1", 
          m.fun "2" 20 false "a2"
        ],
        executeFailed: Just \m -> [ m.fun "2" 20 false "a1" ],
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
        name: "引数が5つの場合", 
        create: \_ -> mock $ [
          "1" :> 10 :> true  :> "a1" :> 2.0 :> false, 
          "2" :> 20 :> false :> "a2" :> 3.0 :> true
        ],
        expected: [
          false, 
          true
        ], 
        execute: \m -> [
          m.fun "1" 10 true  "a1" 2.0, 
          m.fun "2" 20 false "a2" 3.0
        ],
        executeFailed: Just \m -> [ m.fun "2" 20 false "a2" 3.1 ],
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
        name: "引数が6つの場合", 
        create: \_ -> mock $ [
          "1" :> 10 :> true  :> "a1" :> 2.0 :> false :> "b2", 
          "2" :> 20 :> false :> "a2" :> 3.0 :> true  :> "b3"
        ],
        expected: [
          "b2", 
          "b3"
        ], 
        execute: \m -> [
          m.fun "1" 10 true  "a1" 2.0 false, 
          m.fun "2" 20 false "a2" 3.0 true
        ],
        executeFailed: Just \m -> [ m.fun "2" 20 false "a2" 3.0 false ],
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
        name: "引数が7つの場合", 
        create: \_ -> mock $ [
          "1" :> 10 :> true  :> "a1" :> 2.0 :> false :> "b2" :> 200, 
          "2" :> 20 :> false :> "a2" :> 3.0 :> true  :> "b3" :> 300
        ],
        expected: [
          200, 
          300
        ], 
        execute: \m -> [
          m.fun "1" 10 true  "a1" 2.0 false "b2", 
          m.fun "2" 20 false "a2" 3.0 true  "b3"
        ],
        executeFailed: Just \m -> [ m.fun "2" 20 false "a2" 3.0 true "b2" ],
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
        name: "引数が8つの場合", 
        create: \_ -> mock $ [
          "1" :> 10 :> true  :> "a1" :> 2.0 :> false :> "b2" :> 200 :> true,
          "2" :> 20 :> false :> "a2" :> 3.0 :> true  :> "b3" :> 300 :> false
        ],
        expected: [
          true, 
          false
        ], 
        execute: \m -> [
          m.fun "1" 10 true  "a1" 2.0 false "b2" 200, 
          m.fun "2" 20 false "a2" 3.0 true  "b3" 300
        ],
        executeFailed: Just \m -> [ m.fun "2" 20 false "a2" 3.0 true "b3" 200 ],
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
        name: "引数が9つの場合", 
        create: \_ -> mock $ [
          "1" :> 10 :> true  :> "a1" :> 2.0 :> false :> "b2" :> 200 :> true  :> "c3",
          "2" :> 20 :> false :> "a2" :> 3.0 :> true  :> "b3" :> 300 :> false :> "c4"
        ],
        expected: [
          "c3", 
          "c4"
        ], 
        execute: \m -> [
          m.fun "1" 10 true  "a1" 2.0 false "b2" 200 true, 
          m.fun "2" 20 false "a2" 3.0 true  "b3" 300 false
        ],
        executeFailed: Just \m -> [ m.fun "2" 20 false "a2" 3.0 true "b3" 300 true ],
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

    describe "Matcher" do
      mockTest {
        name: "任意の引数を扱う", 
        create: \_ -> mock $ (any :: Param String) :> 11,
        expected: [11, 11, 11], 
        execute: \m -> [m.fun "1233", m.fun "1234", m.fun "2234"],
        executeFailed: Nothing,
        verifyMock: \m -> verify m "1234",
        verifyCount: \m c -> verifyCount m c "1234",
        verifyFailed: \m -> verify m "foo"
      }

      mockTest {
        name: "任意の引数で検証を行う", 
        create: \_ -> mock $ "1234" :> 11,
        expected: 11, 
        execute: \m -> m.fun "1234",
        executeFailed: Just \m -> m.fun "1233",
        verifyMock: \m -> verify m (any :: Param String),
        verifyCount: \m c -> verifyCount m c (any :: Param String),
        verifyFailed: \m -> verify m "not called param"
      }

      mockTest {
        name: "独自の方法で引数を扱う場合", 
        create: \_ -> mock $ matcher (\v -> v > 10) "> 10" :> "Expected",
        expected: "Expected", 
        execute: \m -> m.fun 11,
        executeFailed: Just \m -> m.fun 10,
        verifyMock: \m -> verify m 11,
        verifyCount: \m c -> verifyCount m c 11,
        verifyFailed: \m -> verify m 10
      }

      mockTest {
        name: "独自の方法で検証を行う", 
        create: \_ -> mock $ 10 :> "Expected",
        expected: "Expected", 
        execute: \m -> m.fun 10,
        executeFailed: Just \m -> m.fun 1000,
        verifyMock: \m -> verify m $ matcher (\v -> v < 11) "< 11",
        verifyCount: \m c -> verifyCount m c $ matcher (\v -> v > 9) "> 9",
        verifyFailed: \m -> verify m $ matcher (\v -> v > 11) "> 11"
      }

    describe "Verifyの回数を細かく指定できる" do
      it "指定回数以上" do
        let 
          m = mock $ "a" :> 10
          _ = m.fun "a"
          _ = m.fun "a"
          _ = m.fun "a"
        verifyCount m (GreaterThanEqual 3) "a"
      it "指定回数以下" do
        let 
          m = mock $ "a" :> 10
          _ = m.fun "a"
          _ = m.fun "a"
          _ = m.fun "a"
        verifyCount m (LessThanEqual 3) "a"
      it "指定回数超" do
        let 
          m = mock $ "a" :> 10
          _ = m.fun "a"
          _ = m.fun "a"
          _ = m.fun "a"
        verifyCount m (GreaterThan 2) "a"
      it "指定回数未満" do
        let 
          m = mock $ "a" :> 10
          _ = m.fun "a"
          _ = m.fun "a"
          _ = m.fun "a"
        verifyCount m (LessThan 4) "a"

    describe "MonadのMock" do
      it "Monadを返すことができる1" do
        let
          m = mock $ 1 :> (pure "hoge" :: Identity String)
          _ = m.fun 1
        verify m 1
      
      it "Monadを返すことができる2" do
        let
          -- Monad m のようにする場合、いまどのMonadで動いてるのかわからないといけない(mは駄目で、ちゃんと指定しないといけない)
          findByTitleMock = mock $ "古いタイトル" :> (pure { title: "新しいタイトル" } :: Aff Article)

        result <- findByTitleMock.fun "古いタイトル"

        result `shouldEqual` {title: "新しいタイトル"}
        
        verify findByTitleMock "古いタイトル"
      
      it "Monadを返すことができる3" do
        let
          updateMock = mock $ "新しいtitle" :> (pure unit :: StateT State Aff Unit)
        _ <- runStateT (updateMock.fun "新しいtitle") {article: {title: "Dummy"}} 
        verify updateMock "新しいtitle"

    describe "Cons" do
      describe "Show" do
        it "arg2" do
          show (10 :> true) `shouldEqual` "10, true"
        it "arg3" do
          show ("1" :> false :> [3, 4]) `shouldEqual` "\"1\", false, [3,4]"
      describe "Eq" do
        it "arg2" do
          (1 :> "2") `shouldEqual` (1 :> "2")
        it "arg3" do
          ("1" :> false :> [3, 4]) `shouldEqual` ("1" :> false :> [3, 4])

type Article = {
  title :: String
}

type State = { 
  article :: Article 
}
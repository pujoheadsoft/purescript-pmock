module Test.PMock.Internal.ExpectationSpec (expectationSpec) where

import Prelude

import Test.PMock.Internal.Builder (fun, mockHandle)
import Test.PMock.Internal.Expectation (atLeast, atMost, called, calledInOrder, calledInPartialOrder, never, once, times, verifyExpectations, with)
import Test.PMock.Internal.Param (Param, any, matcher, (:>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

data Opaque = Opaque Int

expectationSpec :: Spec Unit
expectationSpec = describe "Expectation" do
  it "verifies a declared call count with a raw argument" do
    m <- mockHandle $ "Aja" :> 1977
    fun m "Aja" `shouldEqual` 1977

    verifyExpectations m (called once `with` "Aja")

  it "accepts a predicate matcher without Eq or Show" do
    let positive = matcher (\(Opaque n) -> n > 0) "positive opaque" :: Param Opaque
    m <- mockHandle $ positive :> "positive"
    fun m (Opaque 1) `shouldEqual` "positive"

    verifyExpectations m (called once `with` positive)

  it "combines multiple expectations in a do block" do
    m <- mockHandle $ any @String :> true
    fun m "a" `shouldEqual` true
    fun m "a" `shouldEqual` true
    fun m "b" `shouldEqual` true

    verifyExpectations m do
      called (times 2) `with` "a"
      called once `with` "b"
      called never `with` "c"

  it "verifies the total call count without arguments" do
    m <- mockHandle $ any @String :> true
    fun m "a" `shouldEqual` true
    fun m "b" `shouldEqual` true
    fun m "c" `shouldEqual` true

    verifyExpectations m $ called (times 3)

  it "verifies exact and partial call order" do
    m <- mockHandle $ any @String :> true
    fun m "a" `shouldEqual` true
    fun m "b" `shouldEqual` true
    fun m "c" `shouldEqual` true

    verifyExpectations m do
      calledInOrder [ "a", "b", "c" ]
      calledInPartialOrder [ "a", "c" ]

  it "verifies lower and upper count bounds" do
    m <- mockHandle $ any @String :> true
    fun m "a" `shouldEqual` true
    fun m "a" `shouldEqual` true

    verifyExpectations m do
      called (atLeast 2) `with` "a"
      called (atMost 3) `with` "a"

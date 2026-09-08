module Test.PMock.VerifySpec (verifySpec) where

import Prelude

import Test.PMock.Builder (fun, mock)
import Test.PMock.Param (any, (:>))
import Test.PMock.Verify
  ( hasBeenCalledTimesGreaterThan
  , hasBeenCalledTimesGreaterThanEqual
  , hasBeenCalledTimesLessThan
  , hasBeenCalledTimesLessThanEqual
  , hasNotBeenCalledWith
  , with
  )
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (expectError)

verifySpec :: Spec Unit
verifySpec = describe "Verify" do
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

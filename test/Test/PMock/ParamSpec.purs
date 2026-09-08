module Test.PMock.ParamSpec (paramSpec) where

import Prelude

import Test.PMock.Param (and, any, matcher, notEqual, or, param)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

paramSpec :: Spec Unit
paramSpec = describe "Param" do
  it "compares plain parameters" do
    param 42 `shouldEqual` param 42

  it "matches any value" do
    (any @Int == param 42) `shouldEqual` true

  it "matches a predicate" do
    (matcher (_ > 10) "> 10" == param 11) `shouldEqual` true

  it "combines and negates matchers" do
    ((10 `or` 20) == param 20) `shouldEqual` true
    ((matcher (_ >= 10) ">= 10" `and` matcher (_ < 20) "< 20") == param 15)
      `shouldEqual` true
    (notEqual 10 == param 11) `shouldEqual` true

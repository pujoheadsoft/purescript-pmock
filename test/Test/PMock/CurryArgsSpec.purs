module Test.PMock.CurryArgsSpec (curryArgsSpec) where

import Prelude

import Test.PMock.Cons (type (#>), (#>))
import Test.PMock.CurryArgs (curryArgs)
import Test.PMock.Param (Param, value)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

curryArgsSpec :: Spec Unit
curryArgsSpec = describe "CurryArgs" do
  it "builds a one-argument function" do
    let
      f :: Int -> Int
      f = curryArgs \(a :: Param Int) -> value a

    f 42 `shouldEqual` 42

  it "recursively builds a multiple-argument function" do
    let
      f :: Int -> String -> String
      f = curryArgs \(params :: Param Int #> Param String) -> case params of
        a #> b -> show (value a) <> value b

    f 42 "!" `shouldEqual` "42!"

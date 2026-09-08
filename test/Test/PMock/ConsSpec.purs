module Test.PMock.ConsSpec (consSpec) where

import Prelude

import Test.PMock.Cons ((#>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

consSpec :: Spec Unit
consSpec = describe "Cons" do
  describe "Show" do
    it "shows two values" do
      show (10 #> true) `shouldEqual` "10,true"

    it "shows a recursive chain" do
      show ("1" #> false #> [3, 4]) `shouldEqual` "\"1\",false,[3,4]"

  describe "Eq" do
    it "compares two values" do
      (1 #> "2") `shouldEqual` (1 #> "2")

    it "compares a recursive chain" do
      ("1" #> false #> [3, 4]) `shouldEqual` ("1" #> false #> [3, 4])

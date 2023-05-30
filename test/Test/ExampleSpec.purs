module Test.ExampleSpec where

import Prelude

import Test.PMock (fun, matcher, mock, verify, (:>))
import Test.PMock.Param (and)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Example Spec" do
    it "AND Matcher test" do
      let
        m = mock $ matcher (0 < _) "0 < x" `and` matcher (_ < 3) "x < 3" :> "OK"

      fun m 1 `shouldEqual` "OK"
      fun m 2 `shouldEqual` "OK"

      verify m 1
      verify m 2

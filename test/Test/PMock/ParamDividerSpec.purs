module Test.PMock.ParamDividerSpec (paramDividerSpec) where

import Prelude

import Test.PMock.Cons (type (#>), (#>))
import Test.PMock.Param (Param, param)
import Test.PMock.ParamDivider (args, returnValue)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

paramDividerSpec :: Spec Unit
paramDividerSpec = describe "ParamDivider" do
  it "divides one argument from its return value" do
    args (param 1 #> param "result") `shouldEqual` param 1
    returnValue (param 1 #> param "result") `shouldEqual` "result"

  it "recursively divides multiple arguments from the return value" do
    args (param 1 #> param "two" #> param true)
      `shouldEqual` ((param 1 #> param "two") :: Param Int #> Param String)
    returnValue (param 1 #> param "two" #> param true) `shouldEqual` true

module Test.PMock.Internal.ParamDividerSpec (paramDividerSpec) where

import Prelude

import Test.PMock.Internal.Cons (type (#>), (#>))
import Test.PMock.Internal.Param (Param, param)
import Test.PMock.Internal.ParamDivider (args, returnValue)
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

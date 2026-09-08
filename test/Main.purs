module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.PMock.BuilderSpec (builderSpec)
import Test.PMock.ConsSpec (consSpec)
import Test.PMock.CurryArgsSpec (curryArgsSpec)
import Test.PMock.MessageSpec (messageSpec)
import Test.PMock.ParamDividerSpec (paramDividerSpec)
import Test.PMock.ParamSpec (paramSpec)
import Test.PMockSpec (pmockSpec)
import Test.PMock.TypesSpec (typesSpec)
import Test.PMock.VerifySpec (verifySpec)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ specReporter ] do
  pmockSpec
  builderSpec
  verifySpec
  messageSpec
  typesSpec
  consSpec
  paramSpec
  paramDividerSpec
  curryArgsSpec

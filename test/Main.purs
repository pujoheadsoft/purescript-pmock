module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.PMock.Internal.BuilderSpec (builderSpec)
import Test.PMock.Internal.ConsSpec (consSpec)
import Test.PMock.Internal.CurryArgsSpec (curryArgsSpec)
import Test.PMock.Internal.ExpectationSpec (expectationSpec)
import Test.PMock.Internal.FunctionParamsSpec (functionParamsSpec)
import Test.PMock.Internal.MessageSpec (messageSpec)
import Test.PMock.Internal.ParamDividerSpec (paramDividerSpec)
import Test.PMock.ParamSpec (paramSpec)
import Test.PMock.PublicApiSpec (publicApiSpec)
import Test.PMock.ReadmeSpec (readmeSpec)
import Test.PMock.Internal.RegistrySpec (registrySpec)
import Test.PMock.ScopeSpec (scopeSpec)
import Test.PMock.SpecSpec (specSpec)
import Test.PMock.StubSpec (stubSpec)
import Test.PMockSpec (pmockSpec)
import Test.PMock.Internal.TypesSpec (typesSpec)
import Test.PMock.Internal.VerifySpec (verifySpec)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ specReporter ] do
  pmockSpec
  publicApiSpec
  readmeSpec
  builderSpec
  expectationSpec
  functionParamsSpec
  stubSpec
  registrySpec
  scopeSpec
  specSpec
  verifySpec
  messageSpec
  typesSpec
  consSpec
  paramSpec
  paramDividerSpec
  curryArgsSpec

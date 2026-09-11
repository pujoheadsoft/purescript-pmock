module Test.PMock.Internal.RegistrySpec (registrySpec) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Test.PMock.Internal.Registry (lookupRecorder, registerRecorder, unregisterRecorder)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

registrySpec :: Spec Unit
registrySpec = describe "Registry" do
  it "looks up a recorder by function identity" do
    liftEffect do
      let fn = \n -> n + 1
      registerRecorder fn { id: 42 }

      recorder <- lookupRecorder fn
      (map _.id recorder) `shouldEqual` Just 42

  it "returns Nothing for a function that was not registered" do
    liftEffect do
      recorder <- lookupRecorder (\n -> n + 1)

      (map _.id recorder) `shouldEqual` (Nothing :: Maybe Int)

  it "removes a recorder registration" do
    liftEffect do
      let fn = \n -> n + 1
      registerRecorder fn { id: 42 }
      unregisterRecorder fn

      recorder <- lookupRecorder fn
      (map _.id recorder) `shouldEqual` (Nothing :: Maybe Int)

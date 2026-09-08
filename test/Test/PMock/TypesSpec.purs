module Test.PMock.TypesSpec (typesSpec) where

import Prelude

import Data.Maybe (Maybe(..))
import Test.PMock.Types (Mock(..), Verifier(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

typesSpec :: Spec Unit
typesSpec = describe "Types" do
  it "keeps the function and recorded parameters together" do
    let m = Mock Nothing (_ + 1) (Verifier [10, 20])

    case m of
      Mock name f (Verifier calledParams) -> do
        name `shouldEqual` Nothing
        f 1 `shouldEqual` 2
        calledParams `shouldEqual` [10, 20]

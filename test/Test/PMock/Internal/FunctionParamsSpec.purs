module Test.PMock.Internal.FunctionParamsSpec (functionParamsSpec) where

import Prelude

import Effect (Effect)
import Test.PMock.Internal.Cons (type (#>))
import Test.PMock.Internal.FunctionParams (functionParams)
import Test.PMock.Internal.Param (Param)
import Test.Spec (Spec, describe, it)
import Type.Proxy (Proxy(..))

functionParamsSpec :: Spec Unit
functionParamsSpec = describe "FunctionParams" do
  it "resolves one argument, recursive arguments, and Effect" do
    let
      _ = functionParams (Proxy :: Proxy (String -> Int))
        :: Proxy (Param String)
      _ = functionParams (Proxy :: Proxy (String -> Int -> Boolean))
        :: Proxy (Param String #> Param Int)
      _ = functionParams (Proxy :: Proxy (Effect String))
        :: Proxy (Param Unit)
      _ = functionParams
        ( Proxy
            :: Proxy
                 ( Int
                 -> String
                 -> Boolean
                 -> Number
                 -> Array Int
                 -> { six :: Int }
                 -> Int
                 -> String
                 -> Boolean
                 -> Number
                 -> String
                 )
        )
        :: Proxy
             ( Param Int
             #> Param String
             #> Param Boolean
             #> Param Number
             #> Param (Array Int)
             #> Param { six :: Int }
             #> Param Int
             #> Param String
             #> Param Boolean
             #> Param Number
             )
    pure unit

module Test.PMock.Internal.FunctionParams
  ( class FunctionParams
  , functionParams
  ) where

import Prelude (Unit)

import Effect (Effect)
import Test.PMock.Internal.Cons (type (#>))
import Test.PMock.Internal.Param (Param)
import Type.Proxy (Proxy(..))

class FunctionParams :: Type -> Type -> Constraint
class FunctionParams fun params | fun -> params

instance functionParamsEffect ::
  FunctionParams (Effect result) (Param Unit)

instance functionParamsMore ::
  FunctionParams (next -> result) rest =>
  FunctionParams (argument -> next -> result) (Param argument #> rest)

else instance functionParamsOne ::
  FunctionParams (argument -> result) (Param argument)

functionParams
  :: forall fun params
   . FunctionParams fun params
  => Proxy fun
  -> Proxy params
functionParams _ = Proxy

module Test.PMock.ParamDivider
 ( class ParamDivider
 , args
 , return
 , returnValue
 ) where

import Prelude

import Test.PMock.Cons (type (#>), (#>))
import Test.PMock.Param (Param, value)

class ParamDivider params args return | params -> args, params -> return where
  args :: params -> args
  return :: params -> return

instance divider1 :: ParamDivider (Param a #> Param r) (Param a) (Param r) where
  args (a #> _) = a
  return (_ #> r) = r

instance dividerMore ::
  ParamDivider (Param b #> rest) restArgs result =>
  ParamDivider
    (Param a #> Param b #> rest)
    (Param a #> restArgs)
    result where
  args (a #> rest) = a #> args rest
  return (_ #> rest) = return rest

returnValue :: forall params args r. ParamDivider params args (Param r) => params -> r
returnValue = return >>> value

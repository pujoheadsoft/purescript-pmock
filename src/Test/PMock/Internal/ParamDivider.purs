module Test.PMock.Internal.ParamDivider
 ( class ParamDivider
 , class ReturnValue
 , returnValueOf
 , args
 , return
 , returnValue
 ) where

import Prelude

import Test.PMock.Internal.Cons (type (#>), (#>))
import Test.PMock.Internal.Param (Param, Result(..), value)

class ParamDivider params args return | params -> args, params -> return where
  args :: params -> args
  return :: params -> return

instance divider1 :: ParamDivider (Param a #> Param r) (Param a) (Param r) where
  args (a #> _) = a
  return (_ #> r) = r

else
instance dividerResult1 :: ParamDivider (Param a #> Result r) (Param a) (Result r) where
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

class ReturnValue result r | result -> r where
  returnValueOf :: result -> r

instance returnValueParam :: ReturnValue (Param r) r where
  returnValueOf = value

instance returnValueResult :: ReturnValue (Result r) r where
  returnValueOf (Result result) = result

returnValue :: forall params args result r.
  ParamDivider params args result => ReturnValue result r => params -> r
returnValue = return >>> returnValueOf

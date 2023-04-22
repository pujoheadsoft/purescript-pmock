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

instance divider9 :: ParamDivider 
  (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param i #> Param r)
  (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param i) (Param r) where
  args (a #> b #> c #> d #> e #> f #> g #> h #> i #> _) = a #> b #> c #> d #> e #> f #> g #> h #> i
  return (_ #> _ #> _ #> _ #> _ #> _ #> _ #> _ #> _ #> r) = r
else
instance divider8 :: ParamDivider 
  (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param r)
  (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h) (Param r) where
  args (a #> b #> c #> d #> e #> f #> g #> h #> _) = a #> b #> c #> d #> e #> f #> g #> h
  return (_ #> _ #> _ #> _ #> _ #> _ #> _ #> _ #> r) = r
else
instance divider7 :: ParamDivider 
  (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param r)
  (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g) (Param r) where
  args (a #> b #> c #> d #> e #> f #> g #> _) = a #> b #> c #> d #> e #> f #> g
  return (_ #> _ #> _ #> _ #> _ #> _ #> _ #> r) = r
else
instance divider6 :: ParamDivider 
  (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param r)
  (Param a #> Param b #> Param c #> Param d #> Param e #> Param f) (Param r) where
  args (a #> b #> c #> d #> e #> f #> _) = a #> b #> c #> d #> e #> f
  return (_ #> _ #> _ #> _ #> _ #> _ #> r) = r
else
instance divider5 :: ParamDivider 
  (Param a #> Param b #> Param c #> Param d #> Param e #> Param r)
  (Param a #> Param b #> Param c #> Param d #> Param e) (Param r) where
  args (a #> b #> c #> d #> e #> _) = a #> b #> c #> d #> e
  return (_ #> _ #> _ #> _ #> _ #> r) = r
else
instance divider4 :: ParamDivider (Param a #> Param b #> Param c #> Param d #> Param r) (Param a #> Param b #> Param c #> Param d) (Param r) where
  args (a #> b #> c #> d #> _) = a #> b #> c #> d
  return (_ #> _ #> _ #> _ #> r) = r
else
instance divider3 :: ParamDivider (Param a #> Param b #> Param c #> Param r) (Param a #> Param b #> Param c) (Param r) where
  args (a #> b #> c #> _) = a #> b #> c
  return (_ #> _ #> _ #> r) = r
else
instance divider2 :: ParamDivider (Param a #> Param b #> Param r) (Param a #> Param b) (Param r) where
  args (a #> b #> _) = a #> b
  return (_ #> _ #> r) = r
else
instance divider1 :: ParamDivider (Param a #> Param r) (Param a) (Param r) where
  args (a #> _) = a
  return (_ #> r) = r

returnValue :: forall params args r. ParamDivider params args (Param r) => params -> r
returnValue = return >>> value

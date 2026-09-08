module Test.PMock.CurryArgs
  ( class CurryArgs
  , curryArgs
  ) where

import Test.PMock.Cons (Cons(..), type (#>))
import Test.PMock.Param (Param, param)

class CurryArgs args result fun | args result -> fun where
  curryArgs :: (args -> result) -> fun

instance curryOne :: CurryArgs (Param a) result (a -> result) where
  curryArgs f = \a -> f (param a)

instance curryMore ::
  CurryArgs rest result restFun =>
  CurryArgs (Param a #> rest) result (a -> restFun) where
  curryArgs f = \a ->
    curryArgs (\(rest :: rest) -> f (Cons (param a) rest))

module Test.PMock.Internal.CurryArgs
  ( class CurryArgs
  , curryArgs
  ) where

import Test.PMock.Internal.Cons (Cons(..), type (#>))
import Test.PMock.Internal.Param (Param, actual)

class CurryArgs args result fun | args result -> fun where
  curryArgs :: (args -> result) -> fun

instance curryOne :: CurryArgs (Param a) result (a -> result) where
  curryArgs f = \a -> f (actual a)

instance curryMore ::
  CurryArgs rest result restFun =>
  CurryArgs (Param a #> rest) result (a -> restFun) where
  curryArgs f = \a ->
    curryArgs (\(rest :: rest) -> f (Cons (actual a) rest))

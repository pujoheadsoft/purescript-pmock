module Test.PMock.Cons 
  ( Cons(..)
  , (#>)
  , type (#>)
  ) where

import Prelude

data Cons a b = Cons a b

instance showCons :: (Show a, Show b) => Show (Cons a b) where
  show (Cons a b) = (show a) <> "," <> (show b)

instance eqCons :: (Eq a, Eq b) => Eq (Cons a b) where
  eq (Cons a b) (Cons a2 b2) = (eq a a2) && (eq b b2)

infixr 8 type Cons as #>
infixr 8 Cons as #>

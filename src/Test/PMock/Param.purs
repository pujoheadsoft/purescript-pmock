module Test.PMock.Param
  ( Param(..)
  , class ConsGen
  , cons
  , (:>)
  , value
  , param
  , Matcher
  , matcher
  , any
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Test.PMock.Cons (Cons(..))
import Unsafe.Coerce (unsafeCoerce)

data Param v = Param v (Maybe (Matcher v))

instance eqParam :: Eq a => Eq (Param a) where
  eq (Param a (Just (Matcher m1))) (Param b (Just (Matcher m2))) = (m1 a b) && (m2 a b)
  eq (Param a (Just (Matcher m1))) (Param b Nothing)             = m1 a b
  eq (Param a Nothing)             (Param b (Just (Matcher m2))) = m2 a b
  eq (Param a Nothing)             (Param b Nothing)             = a == b

instance showParam :: Show a => Show (Param a) where
  show (Param v _) = show v

param :: forall a. a -> Param a
param a = Param a Nothing

value :: forall v. Param v -> v
value (Param v _) = v


class ConsGen a b r | a -> r, b -> r where
  cons :: a -> b -> r

instance instaneConsGen9 :: ConsGen (Cons a b) (Cons b c) (Cons (Cons a b) (Cons b c)) where
  cons = Cons
else
instance instaneConsGen8 :: ConsGen (Cons a b) (Param b) (Cons (Cons a b) (Param b)) where
  cons = Cons
else
instance instaneConsGen7 :: ConsGen (Param a) (Cons b c) (Cons (Param a) (Cons b c)) where
  cons = Cons
else
instance instaneConsGen6 :: ConsGen a (Cons b c) (Cons (Param a) (Cons b c)) where
  cons a b = Cons (Param a Nothing) b
else
instance instaneConsGen5 :: ConsGen (Cons a b) c (Cons (Cons a b) (Param c)) where
  cons a b = Cons a (Param b Nothing)
else
instance instaneConsGen4 :: ConsGen (Param a) (Param b) (Cons (Param a) (Param b)) where
  cons = Cons
else
instance instaneConsGen3 :: ConsGen a (Param b) (Cons (Param a) (Param b)) where
  cons a b = Cons (Param a Nothing) b
else
instance instaneConsGen2 :: ConsGen (Param a) b (Cons (Param a) (Param b)) where
  cons a b = Cons a (Param b Nothing)
else
instance instaneConsGen :: ConsGen a b (Cons (Param a) (Param b)) where
  cons a b = Cons (param a) (param b)

infixr 8 cons as :>


newtype Matcher v = Matcher (v -> v -> Boolean)

anyMatcher :: forall a. a -> a -> Boolean
anyMatcher _ _ = true

any :: forall a. Param a
any = unsafeCoerce (Param "any" $ Just $ Matcher anyMatcher)

matcher :: forall a. (a -> Boolean) -> String -> Param a
matcher f msg = Param (unsafeCoerce msg) (Just $ Matcher (\_ a -> f a))

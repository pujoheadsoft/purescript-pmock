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
  , anyMatcher
  )
 where

import Prelude

import Data.Maybe (Maybe(..))
import Test.PMock.Cons (Cons(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Param v = Param {
  v :: v,
  matcher :: Maybe (Matcher v)
}

value :: forall v. Param v -> v
value (Param {v, matcher: _}) = v

instance eqParam :: Eq a => Eq (Param a) where
  eq (Param {v: a, matcher: (Just m1)}) (Param {v: b, matcher: (Just m2)}) = (m1 a b) && (m2 a b)
  eq (Param {v: a, matcher: (Just m1)}) (Param {v: b, matcher: Nothing})   = m1 a b
  eq (Param {v: a, matcher: Nothing})   (Param {v: b, matcher: (Just m2)}) = m2 a b
  eq (Param {v: a, matcher: Nothing})   (Param {v: b, matcher: Nothing})   = a == b

instance showParam :: Show a => Show (Param a) where
  show (Param {v, matcher: _}) = show v

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
  cons a b = Cons (Param {v: a, matcher: Nothing}) b
else
instance instaneConsGen5 :: ConsGen (Cons a b) c (Cons (Cons a b) (Param c)) where
  cons a b = Cons a (Param {v: b, matcher: Nothing})
else
instance instaneConsGen4 :: ConsGen (Param a) (Param b) (Cons (Param a) (Param b)) where
  cons = Cons
else
instance instaneConsGen3 :: ConsGen a (Param b) (Cons (Param a) (Param b)) where
  cons a b = Cons (Param {v: a, matcher: Nothing}) b
else
instance instaneConsGen2 :: ConsGen (Param a) b (Cons (Param a) (Param b)) where
  cons a b = Cons a (Param {v: b, matcher: Nothing})
else
instance instaneConsGen :: ConsGen a b (Cons (Param a) (Param b)) where
  cons a b = Cons (param a) (param b)

param :: forall a. a -> Param a
param a = Param {v: a, matcher: Nothing}

infixr 8 cons as :>

type Matcher v = v -> v -> Boolean

anyMatcher :: forall a. a -> a -> Boolean
anyMatcher _ _ = true

any :: forall a. Param a
any = unsafeCoerce Param {v: "any", matcher: Just anyMatcher}

matcher :: forall a. (a -> Boolean) -> String -> Param a
matcher f m = Param {v: unsafeCoerce m, matcher: Just (\_ a -> f a)}
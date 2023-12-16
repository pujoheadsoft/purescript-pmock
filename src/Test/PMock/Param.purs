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
  , class NotMatcher
  , notEqual
  , class LogicalMatcher
  , and
  , or
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
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

any :: forall @a. Param a
any = unsafeCoerce (Param "any" $ Just $ Matcher anyMatcher)

matcher :: forall a. (a -> Boolean) -> String -> Param a
matcher f msg = Param (unsafeCoerce msg) (Just $ Matcher (\_ a -> f a))

class NotMatcher a r | a -> r where
  notEqual :: a -> r

instance instanceNotMatcherParam :: (Eq a, Show a) => NotMatcher (Param a) (Param a) where
  notEqual (Param v m) = Param (unsafeCoerce $ "Not " <> showWithRemoveEscape v) ((\(Matcher f) -> Matcher (\a b -> true /= f a b)) <$> m)
else
instance instanceNotMatcher :: (Eq a, Show a) => NotMatcher a (Param a) where
  notEqual v = Param (unsafeCoerce $ "Not " <> showWithRemoveEscape v) (Just $ Matcher (\_ a -> a /= v))

class LogicalMatcher a b r | a -> r, b -> r where
  or :: a -> b -> r
  and :: a -> b -> r

instance instanceLogicMatcherBothParam :: (Eq a, Show a) => LogicalMatcher (Param a) (Param a) (Param a) where
  or  p1@(Param _ m1) p2@(Param _ m2) = Param (unsafeCoerce $ showWithRemoveEscape p1 <> " || " <> showWithRemoveEscape p2) (composeOr m1 m2)
  and p1@(Param _ m1) p2@(Param _ m2) = Param (unsafeCoerce $ showWithRemoveEscape p1 <> " && " <> showWithRemoveEscape p2) (composeAnd m1 m2)
else
instance instanceLogicMatcherParam :: (Eq a, Show a) => LogicalMatcher (Param a) a (Param a) where
  or  p1@(Param _ m1) a = Param (unsafeCoerce $ showWithRemoveEscape p1 <> " || " <> showWithRemoveEscape a) (composeOr m1 $ Just $ Matcher (\_ v -> v == a))
  and p1@(Param _ m1) a = Param (unsafeCoerce $ showWithRemoveEscape p1 <> " && " <> showWithRemoveEscape a) (composeAnd m1 $ Just $ Matcher (\_ v -> v == a))
else
instance instanceLogicMatcher :: (Eq a, Show a) => LogicalMatcher a a (Param a) where
  or  a1 a2 = Param (unsafeCoerce $ showWithRemoveEscape a1 <> " || " <> showWithRemoveEscape a2) (Just $ Matcher (\_ a -> a == a1 || a == a2))
  and a1 a2 = Param (unsafeCoerce $ showWithRemoveEscape a1 <> " && " <> showWithRemoveEscape a2) (Just $ Matcher (\_ a -> a == a1 && a == a2))

composeOr :: forall a. Maybe (Matcher a) -> Maybe (Matcher a) -> Maybe (Matcher a)
composeOr (Just (Matcher m1)) (Just (Matcher m2)) = Just $ Matcher (\a b -> m1 a b || m2 a b)
composeOr (Just (Matcher m1)) Nothing             = Just $ Matcher (\a b -> m1 a b)
composeOr Nothing             (Just (Matcher m2)) = Just $ Matcher (\a b -> m2 a b)
composeOr Nothing             Nothing             = Nothing

composeAnd :: forall a. Maybe (Matcher a) -> Maybe (Matcher a) -> Maybe (Matcher a)
composeAnd (Just (Matcher m1)) (Just (Matcher m2)) = Just $ Matcher (\a b -> m1 a b && m2 a b)
composeAnd (Just (Matcher m1)) Nothing             = Just $ Matcher (\a b -> m1 a b)
composeAnd Nothing             (Just (Matcher m2)) = Just $ Matcher (\a b -> m2 a b)
composeAnd Nothing             Nothing             = Nothing

showWithRemoveEscape :: forall a. Show a => a -> String
showWithRemoveEscape s = do
  show s # (replace (unsafeRegex "\\\"" global) "")
module Test.PMock.Internal.Param
  ( Param(..)
  , Result(..)
  , class ConsGen
  , cons
  , (:>)
  , value
  , param
  , actual
  , Matcher
  , matcher
  , matcherBy
  , matcher_
  , any
  , class MatchParams
  , MatchParamsOps
  , matchParamsOps
  , matchesParams
  , renderExpectedParams
  , renderActualParams
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
import Partial.Unsafe (unsafeCrashWith)
import Test.PMock.Internal.Cons (Cons(..))

newtype Matcher v = Matcher
  { matches :: v -> Boolean
  , renderExpected :: String
  , renderActual :: Maybe (v -> String)
  }

data Param v
  = Expected (Maybe v) (Matcher v)
  | Actual v

newtype Result v = Result v

derive newtype instance eqResult :: Eq v => Eq (Result v)
derive newtype instance showResult :: Show v => Show (Result v)

instance eqParam :: Eq (Param a) where
  eq expected candidate = case candidateValue candidate of
    Just candidateValue' -> matchesParam expected candidateValue'
    Nothing -> case candidateValue expected of
      Just expectedValue -> matchesParam candidate expectedValue
      Nothing -> renderExpectedParam expected == renderExpectedParam candidate

instance showParam :: Show (Param a) where
  show = renderExpectedParam

param :: forall a. Eq a => Show a => a -> Param a
param a = Expected (Just a) $ Matcher
  { matches: (_ == a)
  , renderExpected: show a
  , renderActual: Just show
  }

actual :: forall a. a -> Param a
actual = Actual

value :: forall v. Param v -> v
value (Expected (Just v) _) = v
value (Actual v) = v
value (Expected Nothing _) = unsafeCrashWith "A matcher cannot be used as a return value"

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
instance instaneConsGen6 :: (Eq a, Show a) => ConsGen a (Cons b c) (Cons (Param a) (Cons b c)) where
  cons a b = Cons (param a) b
else
instance instaneConsGen5 :: ConsGen (Cons a b) c (Cons (Cons a b) (Result c)) where
  cons a b = Cons a (Result b)
else
instance instaneConsGen4 :: ConsGen (Param a) (Param b) (Cons (Param a) (Param b)) where
  cons = Cons
else
instance instaneConsGen3 :: (Eq a, Show a) => ConsGen a (Param b) (Cons (Param a) (Param b)) where
  cons a b = Cons (param a) b
else
instance instaneConsGen2 :: ConsGen (Param a) b (Cons (Param a) (Result b)) where
  cons a b = Cons a (Result b)
else
instance instaneConsGen :: (Eq a, Show a) => ConsGen a b (Cons (Param a) (Result b)) where
  cons a b = Cons (param a) (Result b)

infixr 8 cons as :>

matcher :: forall a. (a -> Boolean) -> String -> Param a
matcher matches renderExpected = matcherBy
  { matches, renderExpected, renderActual: Nothing }

matcher_ :: forall a. (a -> Boolean) -> Param a
matcher_ matches = matcher matches "[some condition]"

matcherBy :: forall a.
  { matches :: a -> Boolean
  , renderExpected :: String
  , renderActual :: Maybe (a -> String)
  } -> Param a
matcherBy specification = Expected Nothing $ Matcher specification

any :: forall @a. Param a
any = matcher (const true) "any"

class MatchParams params where
  matchesParams :: params -> params -> Boolean
  renderExpectedParams :: params -> String
  renderActualParams :: params -> params -> String

type MatchParamsOps params =
  { matches :: params -> params -> Boolean
  , renderExpected :: params -> String
  , renderActual :: params -> params -> String
  }

matchParamsOps :: forall params. MatchParams params => MatchParamsOps params
matchParamsOps =
  { matches: matchesParams
  , renderExpected: renderExpectedParams
  , renderActual: renderActualParams
  }

instance matchParamsParam :: MatchParams (Param a) where
  matchesParams expected candidate = case candidateValue candidate of
    Just candidateValue' -> matchesParam expected candidateValue'
    Nothing -> false
  renderExpectedParams = renderExpectedParam
  renderActualParams = renderActualParam

instance matchParamsResult :: (Eq a, Show a) => MatchParams (Result a) where
  matchesParams (Result expected) (Result candidate) = expected == candidate
  renderExpectedParams (Result expected) = show expected
  renderActualParams _ (Result candidate) = show candidate

instance matchParamsCons :: (MatchParams a, MatchParams b) => MatchParams (Cons a b) where
  matchesParams (Cons expectedHead expectedTail) (Cons actualHead actualTail) =
    matchesParams expectedHead actualHead && matchesParams expectedTail actualTail
  renderExpectedParams (Cons head tail) =
    renderExpectedParams head <> "," <> renderExpectedParams tail
  renderActualParams (Cons expectedHead expectedTail) (Cons actualHead actualTail) =
    renderActualParams expectedHead actualHead <> "," <>
      renderActualParams expectedTail actualTail

class NotMatcher a r | a -> r where
  notEqual :: a -> r

instance instanceNotMatcherParam :: NotMatcher (Param a) (Param a) where
  notEqual p = mapMatcher
    (\m -> not <<< m)
    ("Not " <> renderExpectedParam p)
    p
else
instance instanceNotMatcher :: (Eq a, Show a) => NotMatcher a (Param a) where
  notEqual v = matcher (_ /= v) ("Not " <> showWithRemoveEscape v)

class LogicalMatcher a b r | a -> r, b -> r where
  or :: a -> b -> r
  and :: a -> b -> r

instance instanceLogicMatcherBothParam :: LogicalMatcher (Param a) (Param a) (Param a) where
  or p1 p2 = combine (||) " || " p1 p2
  and p1 p2 = combine (&&) " && " p1 p2
else
instance instanceLogicMatcherParam :: (Eq a, Show a) => LogicalMatcher (Param a) a (Param a) where
  or p1 a = combine (||) " || " p1 (param a)
  and p1 a = combine (&&) " && " p1 (param a)
else
instance instanceLogicMatcher :: (Eq a, Show a) => LogicalMatcher a a (Param a) where
  or a1 a2 = combine (||) " || " (param a1) (param a2)
  and a1 a2 = combine (&&) " && " (param a1) (param a2)

combine :: forall a. (Boolean -> Boolean -> Boolean) -> String -> Param a -> Param a -> Param a
combine operation separator p1 p2 = Expected Nothing $ Matcher
  { matches: \candidate -> operation (matchesParam p1 candidate) (matchesParam p2 candidate)
  , renderExpected: renderExpectedParam p1 <> separator <> renderExpectedParam p2
  , renderActual: case renderer p1 of
      Just render -> Just render
      Nothing -> renderer p2
  }

mapMatcher :: forall a. ((a -> Boolean) -> a -> Boolean) -> String -> Param a -> Param a
mapMatcher transform description p = Expected Nothing $ Matcher
  { matches: transform (matchesParam p)
  , renderExpected: description
  , renderActual: renderer p
  }

matchesParam :: forall a. Param a -> a -> Boolean
matchesParam (Expected _ (Matcher specification)) = specification.matches
matchesParam (Actual _) = const false

candidateValue :: forall a. Param a -> Maybe a
candidateValue (Expected candidate _) = candidate
candidateValue (Actual candidate) = Just candidate

renderExpectedParam :: forall a. Param a -> String
renderExpectedParam (Expected _ (Matcher specification)) = specification.renderExpected
renderExpectedParam (Actual _) = "<actual value>"

renderActualParam :: forall a. Param a -> Param a -> String
renderActualParam expected candidate = case candidateValue candidate of
  Just candidateValue' -> case renderer expected of
    Just render -> render candidateValue'
    Nothing -> "<actual value>"
  Nothing -> "<actual value>"

renderer :: forall a. Param a -> Maybe (a -> String)
renderer (Expected _ (Matcher specification)) = specification.renderActual
renderer _ = Nothing

showWithRemoveEscape :: forall a. Show a => a -> String
showWithRemoveEscape s =
  show s # replace (unsafeRegex "\\\"" global) ""

module Test.PMock.Matcher
  ( Matcher
  , anyMatcher
  ) where

import Prelude

type Matcher v = v -> v -> Boolean

anyMatcher :: forall a. a -> a -> Boolean
anyMatcher _ _ = true


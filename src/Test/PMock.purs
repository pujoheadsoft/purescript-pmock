module Test.PMock
  ( Mock
  , module Builder
  , module Cons
  , module Param
  , module Verify
  ) where

import Test.PMock.Builder
  ( class MockBuilder
  , class MockSequenceBuilder
  , build
  , buildSequence
  , fun
  , mock
  , mockFun
  , mockSequence
  , namedMock
  , namedMockFun
  , namedMockSequence
  ) as Builder
import Test.PMock.Cons (type (#>), Cons(..), (#>)) as Cons
import Test.PMock.Param
  ( class ConsGen
  , class LogicalMatcher
  , class NotMatcher
  , Matcher
  , Param(..)
  , and
  , any
  , cons
  , matcher
  , notEqual
  , or
  , param
  , value
  , (:>)
  ) as Param
import Test.PMock.Types as Types
import Test.PMock.Verify
  ( class Verify
  , class VerifyCount
  , class VerifyOrder
  , CountVerifyMethod(..)
  , VerifyMatchType(..)
  , hasBeenCalledInOrder
  , hasBeenCalledInPartialOrder
  , hasBeenCalledTimes
  , hasBeenCalledTimesGreaterThan
  , hasBeenCalledTimesGreaterThanEqual
  , hasBeenCalledTimesLessThan
  , hasBeenCalledTimesLessThanEqual
  , hasBeenCalledWith
  , hasBeenRunTimes
  , hasNotBeenCalledWith
  , showCalledParams
  , verify
  , verifyCount
  , verifyPartiallySequence
  , verifySequence
  , with
  ) as Verify

type Mock fun params = Types.Mock fun params

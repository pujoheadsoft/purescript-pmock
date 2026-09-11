module Test.PMock.Expectation
  ( module Internal
  ) where

import Test.PMock.Internal.Expectation
  ( Expectations
  , TimesSpec
  , atLeast
  , atMost
  , greaterThan
  , lessThan
  , called
  , calledWith
  , calledInOrder
  , calledInPartialOrder
  , anything
  , inOrderWith
  , inPartialOrderWith
  , never
  , once
  , times
  , with
  ) as Internal

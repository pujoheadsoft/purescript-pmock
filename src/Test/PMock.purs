module Test.PMock
  ( module Builder
  , module Expectation
  , module Param
  , module Scope
  , module Stub
  ) where

import Test.PMock.Builder (Label, label, mock) as Builder
import Test.PMock.Expectation
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
  ) as Expectation
import Test.PMock.Param
  ( Param
  , and
  , any
  , matcher
  , matcherBy
  , matcher_
  , notEqual
  , or
  , param
  , (:>)
  ) as Param
import Test.PMock.Stub
  ( Case
  , Cases
  , class AndThen
  , andThen
  , class OnCase
  , cases
  , onCase
  , stub
  ) as Stub
import Test.PMock.Scope (expects, shouldBeCalled, withMock) as Scope

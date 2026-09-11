module Test.PMock.Internal.Types
  ( Mock(..)
  , MockName
  , Label(..)
  , CalledParamsList
  , Verifier(..)
  , VerifyFailed(..)
  ) where

import Data.Maybe (Maybe)

data Mock fun params = Mock (Maybe MockName) fun (Verifier params)

type MockName = String

newtype Label = Label MockName

type CalledParamsList params = Array params

newtype Verifier params = Verifier (CalledParamsList params)

newtype VerifyFailed = VerifyFailed String

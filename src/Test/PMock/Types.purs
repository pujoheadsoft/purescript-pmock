module Test.PMock.Types
  ( Mock(..)
  , MockName
  , CalledParamsList
  , Verifier(..)
  , VerifyFailed(..)
  ) where

import Data.Maybe (Maybe)

data Mock fun params = Mock (Maybe MockName) fun (Verifier params)

type MockName = String

type CalledParamsList params = Array params

newtype Verifier params = Verifier (CalledParamsList params)

newtype VerifyFailed = VerifyFailed String

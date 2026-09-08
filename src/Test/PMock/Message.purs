module Test.PMock.Message
  ( message
  , messageForMultiMock
  , mockNameLabel
  ) where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Data.String (joinWith)
import Test.PMock.Types (MockName)

message :: forall a. Show a => Maybe MockName -> a -> a -> String
message name expected actual =
  joinWith "\n"
    [ "function" <> mockNameLabel name <> "was not called with expected arguments."
    , "  expected: " <> show expected
    , "  but was : " <> show actual
    ]

messageForMultiMock :: forall a. Show a => Maybe MockName -> Array a -> a -> String
messageForMultiMock name expecteds actual =
  joinWith "\n"
    [ "function" <> mockNameLabel name <> "was not called with expected arguments."
    , "  expected one of the following:"
    , joinWith "\n" $ ("    " <> _) <<< show <$> expecteds
    , "  but was actual:"
    , ("    " <> _) <<< show $ actual
    ]

mockNameLabel :: Maybe MockName -> String
mockNameLabel = fromMaybe " " <<< enclose " " <<< enclose "`"

enclose :: String -> Maybe String -> Maybe String
enclose e = map (\v -> e <> v <> e)

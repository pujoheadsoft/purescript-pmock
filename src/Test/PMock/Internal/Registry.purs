module Test.PMock.Internal.Registry
  ( registerRecorder
  , lookupRecorder
  , unregisterRecorder
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

registerRecorder :: forall fn recorder. fn -> recorder -> Effect Unit
registerRecorder = registerRecorderImpl

lookupRecorder :: forall fn recorder. fn -> Effect (Maybe recorder)
lookupRecorder fn = do
  registered <- hasRecorderImpl fn
  if registered then
    Just <$> lookupRecorderImpl fn
  else
    pure Nothing

unregisterRecorder :: forall fn. fn -> Effect Unit
unregisterRecorder = unregisterRecorderImpl

foreign import registerRecorderImpl :: forall fn recorder. fn -> recorder -> Effect Unit
foreign import hasRecorderImpl :: forall fn. fn -> Effect Boolean
foreign import lookupRecorderImpl :: forall fn recorder. fn -> Effect recorder
foreign import unregisterRecorderImpl :: forall fn. fn -> Effect Unit

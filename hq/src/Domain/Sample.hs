module Domain.Sample where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Env.Cache (MonadCache (readCache))

getSample :: (MonadCache m) => Text -> m Text
getSample = fmap (fromMaybe "Nothing") . readCache

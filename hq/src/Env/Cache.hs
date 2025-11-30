module Env.Cache where

import Data.Text (Text)

class (Monad m) => MonadCache m where
    writeCache :: Text -> Text -> m ()
    readCache :: Text -> m (Maybe Text)

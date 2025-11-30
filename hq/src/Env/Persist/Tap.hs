module Env.Persist.Tap (MonadTaps (..)) where

import Data.Persist.Tap (CreateTap, Tap, TapId)

class (Monad m) => MonadTaps m where
    createTap :: CreateTap -> m Tap
    getTap :: TapId -> m (Maybe Tap)
    deleteTap :: TapId -> m ()

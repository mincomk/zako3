module Data.Model.AudioRequest where

import Data.Persist.Tap (TapId)
import Data.State.Queue (QueueId)
import Data.Text (Text)
import Data.Types
import GHC.Generics (Generic)

data AudioRequest = AudioRequest
    { tapId :: TapId
    , queueId :: QueueId
    , trackContent :: TrackContent
    , trackParameters :: TrackParameters
    , volume :: Volume
    }
    deriving (Show, Eq, Generic)

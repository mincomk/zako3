module Data.Model.TrackRequest where

import Data.Persist.Tap (TapId)
import Data.State.Track (TrackId)
import Data.Text (Text)
import Data.Types (TrackContent, TrackParameters, Volume)
import GHC.Generics (Generic)

newtype TrackToken = TrackToken Text
    deriving (Show, Eq, Generic)

data TrackRequestMeta = TrackRequestMeta
    { tapId :: TapId
    , trackId :: TrackId
    , trackToken :: TrackToken
    , volume :: Volume
    }
    deriving (Show, Eq, Generic)

data TrackRequest = TrackRequest
    { tapId :: TapId
    , trackId :: TrackId
    , trackToken :: TrackToken
    , parameters :: TrackParameters
    , content :: TrackContent
    }
    deriving (Show, Eq, Generic)

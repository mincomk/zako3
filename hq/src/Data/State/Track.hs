module Data.State.Track where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)

newtype TrackId = TrackId Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON)

newtype Track = Track {trackId :: TrackId}
    deriving (Show, Eq, Ord, Generic)

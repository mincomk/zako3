module Data.State.Queue where

import Data.Text (Text)
import GHC.Generics (Generic)

newtype QueueId = QueueId Text
    deriving (Show, Eq, Generic)

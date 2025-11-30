module Data.Persist.Tap where

import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Types (UserId)
import Database.MongoDB
import GHC.Generics (Generic)
import Serialize.Bson (BsonTrans (..))
import Serialize.Str

newtype TapId = TapId Text
    deriving (Show, Eq, Generic)

data TapOccupation = Official | Verified | Base
    deriving (Show, Eq, Generic)

data TapRole = Music | TTS
    deriving (Show, Eq, Generic)

data TapPermissions = Public | OwnerOnly | Whitelist [UserId] | Blacklist [UserId]
    deriving (Show, Eq, Generic)

data Tap = Tap
    { tapId :: TapId
    , tapOwnerId :: UserId
    , tapOccupation :: TapOccupation
    , tapRoles :: [TapRole]
    , tapPermissions :: TapPermissions
    }
    deriving (Show, Eq, Generic)

newtype CreateTap = CreateTap Tap


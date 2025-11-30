module Data.Types (UserId (..), Volume (..), TrackContent (..), TrackParameters (..)) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Serialize.Str

newtype UserId = Discord Text
    deriving (Show, Eq, Generic)

newtype Volume = Volume Integer
    deriving (Show, Eq, Generic)

newtype TrackContent = TrackContent Text
    deriving (Show, Eq, Generic)

newtype TrackParameters = TrackParameters Text
    deriving (Show, Eq, Generic)

instance StrTrans UserId where
    toStr userId = case userId of
        Discord discordId -> "d:" <> discordId

    fromStr s = do
        let parts = T.splitOn ":" s
        (kind, content) <- case parts of
            (kind : content : _) -> Just (kind, content)
            _ -> Nothing

        case kind of
            "d" -> pure $ Discord content
            _ -> Nothing

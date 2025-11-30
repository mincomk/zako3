module Data.Persist.TapTrans where

import Data.Coerce (coerce)
import Data.Persist.Tap
import Data.Text (Text)
import qualified Data.Text as T
import Serialize.Bson
import Serialize.Str

instance StrTrans TapOccupation where
    toStr occu = case occu of
        Official -> "official"
        Verified -> "verified"
        Base -> "base"

    fromStr s = case s of
        "official" -> Just Official
        "verified" -> Just Verified
        "base" -> Just Base
        _ -> Nothing

instance StrTrans TapRole where
    toStr role = case role of
        Music -> "music"
        TTS -> "tts"

    fromStr s = case s of
        "music" -> Just Music
        "tts" -> Just TTS
        _ -> Nothing

instance StrTrans TapId where
    toStr = coerce
    fromStr = Just . TapId

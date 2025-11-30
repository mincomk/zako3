module Domain.AudioRequest where

import Data.Model.AudioRequest (AudioRequest)
import Data.State.Track (TrackId)
import Infrastructure.App (AppM)
import Util.TrackIdGen (generateTrackId, generateTrackToken)

processAudioRequest :: AudioRequest -> AppM TrackId
processAudioRequest = do
    trackId <- generateTrackId
    trackToken <- generateTrackToken

-- TODO

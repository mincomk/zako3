module Domain.AudioRequest where

import Data.Model.AudioRequest (AudioRequest)
import Data.State.Track (TrackId)
import Data.Util.TrackIdGen (generateTrackId, generateTrackToken)
import Infrastructure.App (AppM)

processAudioRequest :: AudioRequest -> AppM TrackId
processAudioRequest = do
    trackId <- generateTrackId
    trackToken <- generateTrackToken

-- TODO

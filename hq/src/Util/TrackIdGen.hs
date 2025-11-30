module Util.TrackIdGen (generateTrackId, generateTrackToken) where

import Control.Monad (replicateM)
import Data.Model.TrackRequest (TrackToken (TrackToken))
import Data.State.Track (TrackId (TrackId))
import Data.Text (Text)
import qualified Data.Text as T
import System.Random (randomRIO)

trackIdLen :: Int
trackIdLen = 32

tokenLen :: Int
tokenLen = 32

alphanumericChars :: [Char]
alphanumericChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

generateAlphanumeric :: Int -> IO Text
generateAlphanumeric len = do
    let charCount = length alphanumericChars

    randomIndices <- replicateM len $ randomRIO (0, charCount - 1)
    let randomString = (alphanumericChars !!) <$> randomIndices

    return $ T.pack randomString

generateTrackId :: IO TrackId
generateTrackId = TrackId <$> generateAlphanumeric trackIdLen

generateTrackToken :: IO TrackToken
generateTrackToken = TrackToken <$> generateAlphanumeric tokenLen

{-# LANGUAGE OverloadedStrings #-}

import qualified Network.MPD as MPD

import Data.Text()

addr :: Maybe String
addr = Just "q150"
port :: Maybe a
port = Nothing
mpd :: MPD.MPD a -> IO (MPD.Response a)
mpd = MPD.withMPD_ addr port

handleResponse :: MPD.Response (Maybe MPD.Song) -> Either MPD.MPDError (Maybe [MPD.Value])
handleResponse (Right (Just content)) = Right $ MPD.sgGetTag MPD.MUSICBRAINZ_TRACKID content
handleResponse (Right Nothing) = Right Nothing
handleResponse (Left e) = Left e

main :: IO ()
main = mpd MPD.currentSong >>= print . handleResponse

{-# LANGUAGE OverloadedStrings #-}

import qualified Network.MPD as MPD

import Data.Text()

addr :: Maybe String
addr = Just "q150"
port :: Maybe a
port = Nothing
mpd :: MPD.MPD a -> IO (MPD.Response a)
mpd = MPD.withMPD_ addr port

handleResponse :: MPD.Response (Maybe MPD.Song) -> IO ()
handleResponse (Left e) = print e
handleResponse (Right content) = print $ fmap getMBIDfromResponse content

getMBIDfromResponse :: MPD.Song -> Maybe [MPD.Value]
getMBIDfromResponse = MPD.sgGetTag MPD.MUSICBRAINZ_TRACKID

main :: IO ()
main = mpd MPD.currentSong >>= handleResponse

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
handleResponse (Right content) = print $ getMBIDfromResponse content

getMBIDfromResponse :: Maybe MPD.Song -> Maybe [MPD.Value]
getMBIDfromResponse (Just song) = MPD.sgGetTag MPD.MUSICBRAINZ_TRACKID song
getMBIDfromResponse _ = Nothing

main :: IO ()
{-main = mpd MPD.currentSong >>= \resp -> print $ getMBIDfromResponse resp-}
main = do
       resp <- mpd MPD.currentSong
       handleResponse resp 

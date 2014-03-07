{-# LANGUAGE OverloadedStrings #-}

import qualified Network.MPD as MPD

import Data.Text()

addr :: Maybe String
addr = Just "q150"
port :: Maybe a
port = Nothing
mpd :: MPD.MPD a -> IO (MPD.Response a)
mpd = MPD.withMPD_ addr port

getMBIDfromResponse :: MPD.Response (Maybe MPD.Song) -> Maybe [MPD.Value]
getMBIDfromResponse (Right (Just song)) = MPD.sgGetTag MPD.MUSICBRAINZ_TRACKID song
getMBIDfromResponse _ = Nothing

main :: IO ()
{-main = mpd MPD.currentSong >>= \resp -> print $ getMBIDfromResponse resp-}
main = do
       resp <- mpd MPD.currentSong
       print $ getMBIDfromResponse resp 

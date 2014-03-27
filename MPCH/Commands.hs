module MPCH.Commands where

import qualified Data.ByteString.Char8 as C
import qualified Network.MPD as MPD
import qualified Text.Show.Pretty as PP

import Data.ByteString.UTF8 (fromString)
import           MPCH.Config (Config)
import           MPCH.MPD (mpd)

type CommandFunc = Config -> [String] -> IO ()

defaultCommand :: CommandFunc
defaultCommand _ _ =  print "unknown command"

currentSong :: CommandFunc
currentSong config _ = mpd config MPD.currentSong >>= either (error . show) printAllTags

nextSong :: CommandFunc
nextSong config _ = mpd config MPD.next >>= eitherError (currentSong config [])

prevSong :: CommandFunc
prevSong config _ = mpd config MPD.previous >>= eitherError (currentSong config [])

setVolume :: CommandFunc
setVolume config (v:_) = case head v of
                             '+' -> changeVolume v
                             '-' -> changeVolume v
                             _ -> setAbsoluteVolume $ read v
                             where
                                 changeVolume amount = do
                                     resp <- mpd config MPD.status
                                     either print (setAbsoluteVolume . (+ change) . MPD.stVolume) resp
                                     where change = read amount
                                 setAbsoluteVolume value = mpd config (MPD.setVolume value) >>= eitherError (currentSong config [])

status :: CommandFunc
status config _ = mpd config MPD.status >>= either print (putStrLn . PP.ppShow) >> currentSong config []

-- If the second argument is a Left, it will be printed, otherwise, the
-- first argument will be called.
eitherError :: Show a => IO () -> Either a t -> IO ()
eitherError _ (Left e) = print e
eitherError f (Right _) = f

tags :: [MPD.Metadata]
tags = [MPD.MUSICBRAINZ_TRACKID, MPD.Artist, MPD.Album, MPD.Title]

printAllTags :: Maybe MPD.Song -> IO ()
printAllTags Nothing = print "No song is playing"
printAllTags (Just song) = mapM_ getAndPrint tags
    where
        getAndPrint tag = do
            let tagName = show tag
                value = getTag song tag
            printTag tagName value

printTag :: String -> Maybe [MPD.Value] -> IO ()
printTag _ Nothing = print "meep"
printTag tagName (Just value) = do
        C.putStr $ fromString tagName
        C.putStr $ fromString ": "
        C.putStrLn $ fromString $ firstElem
    where firstElem = MPD.toString $ head value

getTag :: MPD.Song -> MPD.Metadata -> Maybe [MPD.Value]
getTag song tag = MPD.sgGetTag tag song

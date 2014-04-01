module MPCH.Commands where

import qualified Network.MPD as MPD
import qualified Text.Show.Pretty as PP

import           Data.List (intercalate)
import           MPCH.Config (Config)
import           MPCH.MPD (mpd)

type CommandFunc = Config -> [String] -> IO (String)

defaultCommand :: CommandFunc
defaultCommand _ _ =  return "unknown command"

currentSong :: CommandFunc
currentSong config _ = mpd config MPD.currentSong >>= return . (either show allTags)

-- | Calls 'mpd' with the first argument, throwing away its return value and
--   then calls 'currentSong'
currentSongWrapper :: MPD.MPD a -> Config -> t -> IO String
currentSongWrapper mpdfun config _ = mpd config mpdfun >>= either (return . show) (\_ -> currentSong config [])

nextSong :: CommandFunc
nextSong = currentSongWrapper MPD.next

prevSong :: CommandFunc
prevSong = currentSongWrapper MPD.previous

setVolume :: CommandFunc
setVolume config [] = status config []
setVolume config (v:_) = case head v of
                             '+' -> changeVolume v
                             '-' -> changeVolume v
                             _ -> setAbsoluteVolume $ read v
                             where
                                 changeVolume amount = do
                                     resp <- mpd config MPD.status
                                     either (return . show) (setAbsoluteVolume . (+ change) . MPD.stVolume) resp
                                     where change = read amount
                                 setAbsoluteVolume value = mpd config (MPD.setVolume value) >>= eitherReturn (currentSong config [])
                                 eitherReturn _ (Left e) = return $ show e
                                 eitherReturn f (Right _) = f

status :: CommandFunc
status config _ = mpd config MPD.status >>= either print (putStrLn . PP.ppShow) >> currentSong config []

toggle :: CommandFunc
toggle config _ = mpd config MPD.status >>= either (return . show) (doToggle . MPD.stState)
    where doToggle MPD.Playing = mpd config (MPD.pause True) >> st
          doToggle _ = mpd config (MPD.play Nothing) >> st
          st = status config []

tags :: [MPD.Metadata]
tags = [MPD.MUSICBRAINZ_TRACKID, MPD.Artist, MPD.Album, MPD.Title]

allTags :: Maybe MPD.Song -> String
allTags Nothing = "No song is playing"
allTags (Just song) = intercalate "\n" $ map getTag_ tags
    where
        getTag_ tag = preparePrintableTag tagName value
            where
                tagName = show tag
                value = getTag song tag

preparePrintableTag :: String -> Maybe [MPD.Value] -> String
preparePrintableTag _ Nothing = "meep"
preparePrintableTag tagName (Just value) =
        tagName ++ ": " ++ firstElem
    where firstElem = MPD.toString $ head value

getTag :: MPD.Song -> MPD.Metadata -> Maybe [MPD.Value]
getTag = flip MPD.sgGetTag

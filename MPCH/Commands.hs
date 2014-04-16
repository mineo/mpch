module MPCH.Commands where

import qualified Network.MPD as MPD
import qualified Text.Show.Pretty as PP

import           Control.Monad (liftM)
import           Data.List (intercalate)
import           MPCH.Config (Config)
import           MPCH.MPD (mpd)

type CommandFunc = Config -> [String] -> IO String

defaultCommand :: CommandFunc
defaultCommand _ _ =  return "unknown command"

currentSong :: CommandFunc
currentSong config _ = liftM (either show allTags) (mpd config MPD.currentSong)

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
status config _ = do
    st <- mpd config MPD.status
    either (return . show) handleStatus st
        where handleStatus st = do
              let showSt = PP.ppShow st
              cs <- currentSong config []
              return $ showSt ++ "\n" ++  cs

toggle :: CommandFunc
toggle config _ = mpd config MPD.status >>= either (return . show) (doToggle . MPD.stState)
    where doToggle MPD.Playing = mpd config (MPD.pause True) >> st
          doToggle _ = mpd config (MPD.play Nothing) >> st
          st = status config []

stToggleWrapper :: (MPD.Status -> Bool) -> (Bool -> MPD.MPD a) -> Config -> [String]-> IO String
stToggleWrapper bfun mpdfun config args = case arg of
        Nothing -> execToggle
        Just x -> mpd config (mpdfun x) >> st
    where arg = case args of
            (x:_) -> stringToBool x
            _ -> Nothing
          st = status config []
          execToggle = mpd config MPD.status >>= either (return . show) (doToggle . bfun)
          doToggle True = mpd config (mpdfun False) >> st
          doToggle False = mpd config (mpdfun True) >> st

single :: CommandFunc
single = stToggleWrapper MPD.stSingle MPD.single

consume :: CommandFunc
consume = stToggleWrapper MPD.stConsume MPD.consume

random :: CommandFunc
random = stToggleWrapper MPD.stRandom MPD.random

mpdrepeat :: CommandFunc
mpdrepeat = stToggleWrapper MPD.stRepeat MPD.repeat

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

-- | Maps mpcs command line booleans "on" and "off" to their 'Bool'
--   counterparts or 'Nothing' if the argument is invalid.
stringToBool :: String -> Maybe Bool
stringToBool "on" = Just True
stringToBool "off" = Just False
stringToBool _ = Nothing
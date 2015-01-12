module MPCH.Commands where

import qualified Network.MPD as MPD
import qualified Text.Show.Pretty as PP

import           Control.Monad (liftM)
import           Data.List (intercalate)
import           MPCH.Config (Config)
import           MPCH.MPD (mpd)
import           Network.MPD.Core (getVersion)

type CommandFunc = Config -> [String] -> IO String

defaultCommand :: CommandFunc
defaultCommand _ _ =  return "unknown command"

currentSong :: CommandFunc
currentSong config _ = liftM (either show allTags) (mpd config MPD.currentSong)

-- | Calls 'mpd' with the first argument, throwing away its return value in
--   case of success and then calls 'currentSong'
currentSongWrapper :: MPD.MPD a -> Config -> t -> IO String
currentSongWrapper mpdfun config _ = mpd config mpdfun >>= either retShow doCurrentSong
    where doCurrentSong _ = currentSong config []

nextSong :: CommandFunc
nextSong = currentSongWrapper MPD.next

prevSong :: CommandFunc
prevSong = currentSongWrapper MPD.previous

setVolume :: CommandFunc
setVolume config [] = status config []
setVolume config (v:_) = case head v of
                             '+' -> changeVolume v
                             '-' -> changeVolume v
                             _ -> setAbsoluteVolume $ MPD.Volume $ read v
                             where
                                 changeVolume amount = do
                                     resp <- mpd config MPD.status
                                     either (return . show) (\status -> maybe
                                                                        (return "foo")
                                                                        (setAbsoluteVolume . (+change))
                                                                        (MPD.stVolume status)
                                                                        )
                                       resp
                                     where change = MPD.Volume $ read amount
                                 setAbsoluteVolume value = mpd config (MPD.setVolume value) >>= eitherReturn (currentSong config [])
                                 eitherReturn _ (Left e) = retShow e
                                 eitherReturn f (Right _) = f

status :: CommandFunc
status config _ = do
    st <- mpd config MPD.status
    either (return . show) handleStatus st
        where handleStatus st = do
              let showSt = PP.ppShow st
              cs <- currentSong config []
              return $ showSt ++ "\n" ++  cs


-- | Executes the first argument and, if that was successful, 'status'.
statusWrapper :: (Config -> [String] -> IO (MPD.Response a)) -> CommandFunc
statusWrapper f config args = f config args >>= either retShow (\_ -> status config [])

stToggleWrapper :: (MPD.Status -> Bool) -> (Bool -> MPD.MPD a) -> Config -> [String] -> IO (MPD.Response a)
stToggleWrapper bfun mpdfun config args = case arg of
        Nothing -> execToggle
        Just x -> mpd config (mpdfun x)
    where arg = case args of
            (x:_) -> stringToBool x
            _ -> Nothing
          execToggle = do
            resp <- mpd config MPD.status
            either (return . Left) (doToggle . bfun) resp
          doToggle True = mpd config (mpdfun False)
          doToggle False = mpd config (mpdfun True)

consume :: CommandFunc
consume = statusWrapper $ stToggleWrapper MPD.stConsume MPD.consume

mpdrepeat :: CommandFunc
mpdrepeat = statusWrapper $ stToggleWrapper MPD.stRepeat MPD.repeat

random :: CommandFunc
random = statusWrapper $ stToggleWrapper MPD.stRandom MPD.random

single :: CommandFunc
single = statusWrapper $ stToggleWrapper MPD.stSingle MPD.single

toggleImpl :: Config -> [String] -> IO (MPD.Response ())
toggleImpl config _ = mpd config MPD.status >>= either (return . Left) (doToggle . MPD.stState)
    where doToggle MPD.Playing = mpd config (MPD.pause True)
          doToggle _ = mpd config (MPD.play Nothing)

toggle :: CommandFunc
toggle = statusWrapper toggleImpl

version :: CommandFunc
version config _ = mpd config getVersion >>= either retShow (return . dottedVersion)
    where dottedVersion (major, minor, patch) = intercalate "." $ map show [major, minor, patch]

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

retShow :: (Monad m, Show a) => a -> m String
retShow = return . show

-- | Maps mpcs command line booleans "on" and "off" to their 'Bool'
--   counterparts or 'Nothing' if the argument is invalid.
stringToBool :: String -> Maybe Bool
stringToBool "on" = Just True
stringToBool "off" = Just False
stringToBool _ = Nothing
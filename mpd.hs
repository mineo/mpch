{-# LANGUAGE OverloadedStrings #-}

import qualified Network.MPD as MPD

import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import System.Console.GetOpt (OptDescr(Option), ArgDescr(NoArg, OptArg, ReqArg), getOpt, ArgOrder(Permute), usageInfo)
import System.Environment (getArgs)


data Config = Config {
    host :: Maybe String,
    port :: Maybe String
}
    deriving Show

defaultConfig :: Config
defaultConfig = Config {
    host = Nothing,
    port = Nothing
}

options :: [OptDescr (Config -> Config)]
options =
 [
    Option [] ["host"] (OptArg doHost "HOST") "mpd host",
    Option [] ["port"] (OptArg doPort "PORT") "mpd port"
 ]
    where doHost arg opt = opt { host = arg }
          doPort arg opt = opt { port = arg }

data Command = Command {
    name :: String,
    f :: Config -> IO ()
}

commands :: [Command]
commands = [Command "currentsong" currentSong]

mpd :: MPD.MPD a -> Config -> IO (MPD.Response a)
mpd action config = MPD.withMPD_ h p action
    where h = host config
          p = port config


handleArgs :: ([Config -> Config], [String], [String]) -> IO ()
handleArgs opts = case opts of
                 (args, noptions, []) -> do
                    let config = configure defaultConfig args
                    dispatchArgs config noptions
                 (_, _, errs) ->
                    error $ concat errs ++ usageInfo "" options
                where dispatchArgs _ [] = print "no command specified"
                      dispatchArgs config noptions = mapM_ (applyArg config) noptions

applyArg :: Config -> String -> IO ()
applyArg config noption = doIfMatch config commands noption

doIfMatch :: Config -> [Command] -> String -> IO ()
doIfMatch _ [] _ = print "no command specified"
doIfMatch config (x:xs) commandname
        | commandname == name x = (f x) config
        | otherwise = doIfMatch config xs commandname

configure :: Config -> [Config -> Config] -> Config
configure = foldl (\cfg x -> x cfg)

tags :: [MPD.Metadata]
tags = [MPD.MUSICBRAINZ_TRACKID, MPD.Artist, MPD.Album, MPD.Title]

currentSong :: Config -> IO ()
currentSong config = mpd MPD.currentSong config >>= either (error . show) printAllTags

printAllTags :: Maybe MPD.Song -> IO ()
printAllTags Nothing = print "No song is playing"
printAllTags (Just song) = mapM_ (printTag . getTag song) tags

printTag :: Maybe [MPD.Value] -> IO ()
printTag Nothing = print "meep"
printTag (Just value) = printFirstElem value
    where printFirstElem = print . MPD.toUtf8 . head

getTag :: MPD.Song -> MPD.Metadata -> Maybe [MPD.Value]
getTag song tag = MPD.sgGetTag tag song

main :: IO ()
main = do
        args <- getArgs
        let parsedArgs = parseArgs args
        let handledArgs = handleArgs parsedArgs
        handledArgs
    where parseArgs = getOpt Permute options

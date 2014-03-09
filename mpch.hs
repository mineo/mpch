{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as M
import qualified Network.MPD as MPD

import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import System.Console.GetOpt (OptDescr(Option), ArgDescr(NoArg, OptArg, ReqArg), getOpt, ArgOrder(Permute), usageInfo)
import System.Environment (getArgs)


data Config = Config {
    host :: Maybe String,
    port :: Maybe String,
    password :: Maybe MPD.Password
}
    deriving Show

defaultConfig :: Config
defaultConfig = Config {
    host = Nothing,
    port = Nothing,
    password = Nothing
}

options :: [OptDescr (Config -> Config)]
options =
 [
    Option [] ["host"] (OptArg doHost "HOST") "mpd host",
    Option [] ["port"] (OptArg doPort "PORT") "mpd port",
    Option [] ["password"] (OptArg doPassword "PASSWORD") "mpd password"
 ]
    where doHost arg opt = opt { host = arg }
          doPort arg opt = opt { port = arg }
          doPassword arg opt = opt { password = arg }

data Command = Command {
    f :: Config -> IO ()
}

defaultCommand = Command (\_ -> print "unknown command")

commands :: M.Map String Command
commands = M.fromList[("currentsong", Command currentSong),
            ("next", Command nextSong),
            ("prev", Command prevSong)]

mpd :: MPD.MPD a -> Config -> IO (MPD.Response a)
mpd action config = MPD.withMPD_ h p $ doPw pw >> action
    where h = host config
          p = port config
          pw = password config
          doPw = maybe (return ()) MPD.password


handleArgs :: ([Config -> Config], [String], [String]) -> IO ()
handleArgs opts = case opts of
                 (args, noptions, []) -> do
                    let config = configure defaultConfig args
                    dispatchArgs config noptions
                 (_, _, errs) ->
                    error $ concat errs ++ usageInfo "mpd [OPTION] command" options
                where dispatchArgs _ [] = print "no command specified"
                      dispatchArgs config (subcommand:args) = applyArg config subcommand

applyArg :: Config -> String -> IO ()
applyArg config noption = doIfMatch config commands noption

doIfMatch :: Config -> M.Map String Command -> String -> IO ()
doIfMatch config commands commandname = f commandFun config
    where commandFun = M.findWithDefault defaultCommand commandname commands

configure :: Config -> [Config -> Config] -> Config
configure = foldl (\cfg x -> x cfg)

tags :: [MPD.Metadata]
tags = [MPD.MUSICBRAINZ_TRACKID, MPD.Artist, MPD.Album, MPD.Title]

currentSong :: Config -> IO ()
currentSong config = mpd MPD.currentSong config >>= either (error . show) printAllTags

nextSong :: Config -> IO ()
nextSong config = do
        mpd MPD.next config >>= either (error . show) showSong
    where showSong _ = currentSong config

prevSong :: Config -> IO ()
prevSong config = do
        mpd MPD.previous config >>= either (error . show) showSong
    where showSong _ = currentSong config

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

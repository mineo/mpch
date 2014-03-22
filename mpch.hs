{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import qualified Network.MPD as MPD

import Data.ByteString.UTF8 (fromString)
import System.Console.GetOpt (OptDescr(Option), ArgDescr(OptArg), getOpt, ArgOrder(Permute), usageInfo)
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
    f :: Config -> [String] -> IO ()
}

defaultCommand :: Command
defaultCommand = Command (\_ _ -> print "unknown command")

commands :: M.Map String Command
commands = M.fromList[
            ("currentsong", Command currentSong),
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
                      (_, [], _) ->
                          putStrLn $ "no command specified\n" ++ usage
                      (args, (subcommand:commandargs), []) -> do
                          let config = configure defaultConfig args
                          execCommand config subcommand commandargs
                      (_, _, errs) ->
                           error $ concat errs ++ usage
    where usage = usageInfo "mpch [OPTION] command" options ++ "where command is one of: " ++ commandnames
          commandnames = unwords $ M.keys commands

execCommand :: Config -> String -> [String] -> IO ()
execCommand config commandname args = commandFun config args
    where commandFun = f $ M.findWithDefault defaultCommand commandname commands

configure :: Config -> [Config -> Config] -> Config
configure = foldl (\cfg x -> x cfg)

tags :: [MPD.Metadata]
tags = [MPD.MUSICBRAINZ_TRACKID, MPD.Artist, MPD.Album, MPD.Title]

currentSong :: Config -> [String] -> IO ()
currentSong config _ = mpd MPD.currentSong config >>= either (error . show) printAllTags

nextSong :: Config -> [String] -> IO ()
nextSong config _ = mpd MPD.next config >>= eitherError (currentSong config [])

prevSong :: Config -> [String] -> IO ()
prevSong config _ = mpd MPD.previous config >>= eitherError (currentSong config [])

eitherError :: Show a => IO () -> Either a t -> IO ()
eitherError _ (Left e) = (print . show) e
eitherError f (Right _) = f

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

main :: IO ()
main = do
        args <- getArgs
        let parsedArgs = parseArgs args
        let handledArgs = handleArgs parsedArgs
        handledArgs
    where parseArgs = getOpt Permute options

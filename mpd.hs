{-# LANGUAGE OverloadedStrings #-}

import qualified Network.MPD as MPD

import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import System.Console.GetOpt (OptDescr(Option), ArgDescr(NoArg, OptArg, ReqArg), getOpt, ArgOrder(Permute), usageInfo)
import System.Environment (getArgs)


data Config = Config {
    host :: Maybe String,
    port :: Maybe String,
    artistTag :: Maybe String,
    titleTag :: Maybe String,
    love :: Bool
}
    deriving Show

defaultConfig :: Config
defaultConfig = Config {
    host = Nothing,
    port = Nothing,
    artistTag = Nothing,
    titleTag = Nothing,
    love = False
}
options :: [OptDescr (Config -> Config)]
options =
 [
    Option [] ["host"] (OptArg doHost "HOST") "mpd host",
    Option [] ["port"] (OptArg doPort "PORT") "mpd port",
    Option ['l'] ["love"] (NoArg (\opt -> opt { love = True })) "love the track",
    Option ['a'] ["artisttag"] (ReqArg doArtistTag "artisttag") "artist tag",
    Option ['t'] ["titletag"] (ReqArg doTitleTag "titletag") "title tag"
 ]
    where doHost arg opt = opt { host = arg }
          doPort arg opt = opt { port = arg }
          doArtistTag arg opt = opt {artistTag = Just arg}
          doTitleTag arg opt = opt {titleTag = Just arg}

dispatchList :: [(Config -> IO (), Config -> Bool)]
dispatchList = [(loveTrack, love), (tagTrack, (\c -> isJust $ titleTag c)), (tagArtist, (\c -> isJust $ artistTag c))]


mpd :: MPD.MPD a -> Config -> IO (MPD.Response a)
mpd action config = MPD.withMPD_ h p action
    where h = host config
          p = port config


handleArgs :: ([Config -> Config], t, [[Char]]) -> [IO ()]
handleArgs opts = case opts of
                 (args, _, []) -> do
                    let config = configure defaultConfig args
                    dispatchArgs config
                 (_, _, errs) ->
                    error $ concat errs ++ usageInfo "" options
                where dispatchArgs config = map (applyArg config) dispatchList
                      applyArg config (f, predicate) = when (predicate config) $ f config


configure :: Config -> [Config -> Config] -> Config
configure = foldl (\cfg x -> x cfg)

abortOnNothing :: Maybe t -> [Char] -> IO ()
abortOnNothing Nothing m = error m
abortOnNothing _ _ = return ()

loveTrack :: Config -> IO ()
loveTrack config = do
        resp <- mpd MPD.currentSong config
        let m = either (error . show) (getTag MPD.MUSICBRAINZ_TRACKID) resp
        abortOnNothing m "The song has no mbid"
        let artistname = either (error . show) (getTag MPD.Artist) resp
        abortOnNothing artistname "The song has no artist"
        printFirstElem m
        printFirstElem artistname
    where convertMPDMaybe a = fromJust a
          printFirstElem = print . MPD.toUtf8 . head . convertMPDMaybe

tagTrack :: Config -> IO ()
tagTrack = undefined

tagArtist :: Config -> IO ()
tagArtist = undefined

getTag :: MPD.Metadata -> Maybe MPD.Song-> Maybe [MPD.Value]
getTag t r = maybe Nothing (MPD.sgGetTag t) r

main :: IO [()]
main = do
        args <- getArgs
        let parsedArgs = parseArgs args
        let handledArgs = handleArgs parsedArgs
        sequence handledArgs
    where parseArgs = getOpt Permute options

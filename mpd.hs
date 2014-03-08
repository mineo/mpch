{-# LANGUAGE OverloadedStrings #-}

import qualified Network.MPD as MPD

import Data.Maybe (maybe, fromMaybe, isJust)
import Data.Text ()
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

dispatchList :: [(t, Config -> Bool)]
dispatchList = [(loveTrack, love), (tagTrack, (\c -> isJust $ titleTag c)), (tagArtist, (\c -> isJust $ artistTag c))]


mpd :: MPD.MPD a -> Config -> IO (MPD.Response a)
mpd action config = MPD.withMPD_ h p action
    where h = host config
          p = port config


handleArgs :: ([Config -> Config], t, [[Char]]) -> [b]
handleArgs opts = case opts of
                 (args, _, []) -> do
                    let config = configure defaultConfig args
                    dispatchArgs config
                 (_, _, errs) ->
                    error $ concat errs ++ usageInfo "" options
                where dispatchArgs config = map (applyArg config) dispatchList
                      applyArg config (f, predicate) = case predicate config of True -> f config


configure :: Config -> [Config -> Config] -> Config
configure = foldl (\cfg x -> x cfg)

loveTrack = undefined

tagTrack = undefined

tagArtist = undefined

main :: IO ()
main = do
        args <- getArgs
        let parsedArgs = parseArgs args
        let handledArgs = handleArgs parsedArgs
        let config = configure defaultConfig handledArgs
        resp <- mpd MPD.currentSong config
        print config
        either print (print . (fromMaybe [MPD.Value ""] . getTag)) resp
    where parseArgs = getOpt Permute options
          getTag = maybe Nothing (MPD.sgGetTag MPD.Album)

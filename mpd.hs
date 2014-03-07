{-# LANGUAGE OverloadedStrings #-}

import qualified Network.MPD as MPD

import Data.Maybe (fromMaybe)
import Data.Text ()
import System.Console.GetOpt (OptDescr(Option), ArgDescr(OptArg), getOpt, ArgOrder(Permute))
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

doHost :: Maybe String -> Config -> Config
doHost arg opt = opt { host = arg }

doPort :: Maybe String -> Config -> Config
doPort arg opt = opt { port = arg }

mpd :: MPD.MPD a -> Config -> IO (MPD.Response a)
mpd action config = MPD.withMPD_ h p action
    where h = host config
          p = port config

parseArgs :: [String] -> ([(Config -> Config)], [String], [String])
parseArgs argv = getOpt Permute options argv

handleArgs :: (b, t, [[a]]) -> Either [a] b
handleArgs opts = case opts of
                 (args, _, []) -> do
                     Right args
                 (_, _, errs) -> do
                     Left $ concat errs

handleResponse :: MPD.Response (Maybe MPD.Song) -> Either MPD.MPDError (Maybe [MPD.Value])
handleResponse (Right (Just content)) = Right $ MPD.sgGetTag MPD.MUSICBRAINZ_TRACKID content
handleResponse (Right Nothing) = Right Nothing
handleResponse (Left e) = Left e

configure :: Config -> [(Config -> Config)] -> Config
configure cfg (x:xs) = configure (x cfg) xs
configure cfg [] = cfg

main :: IO ()
main = do
        args <- getArgs
        let parsedArgs = handleArgs $ parseArgs args -- parsedArgs ist Either [Char] [(Config -> Config)]
        let config = either (\_ -> defaultConfig) (configure defaultConfig) parsedArgs --
        print config
        mpd MPD.currentSong config >>= print . handleResponse

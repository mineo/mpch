module MPCH.Config where

import qualified Network.MPD as MPD

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

configure :: Config -> [Config -> Config] -> Config
configure = foldl (\cfg x -> x cfg)

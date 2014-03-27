module MPCH.MPD where

import           MPCH.Config (Config, host, password, port)
import qualified Network.MPD as MPD

mpd :: Config -> MPD.MPD a -> IO (MPD.Response a)
mpd config action = MPD.withMPD_ h p $ doPw pw >> action
    where h = host config
          p = port config
          pw = password config
          doPw = maybe (return ()) MPD.password

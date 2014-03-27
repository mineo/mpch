import qualified Data.Map as M

import System.Console.GetOpt (OptDescr(Option), ArgDescr(OptArg), getOpt, ArgOrder(Permute), usageInfo)
import System.Environment (getArgs)

import MPCH.Commands
import MPCH.Config

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


commands :: M.Map String Command
commands = M.fromList[
            ("currentsong", Command currentSong),
            ("next", Command nextSong),
            ("prev", Command prevSong),
            ("volume", Command setVolume)]



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



main :: IO ()
main = do
        args <- getArgs
        let parsedArgs = parseArgs args
        let handledArgs = handleArgs parsedArgs
        handledArgs
    where parseArgs = getOpt Permute options

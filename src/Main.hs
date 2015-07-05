module Main where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Version (showVersion)
import Language.JavaScript.Parser
import System.Console.GetOpt
import System.Environment (getArgs)
import System.FilePath.Posix (replaceExtension, takeFileName)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy.Char8 as BS

import Config
import Injector (inject)
import Paths_sjsp (version)

main :: IO ()
main = command <$> getOpt Permute options =<< getArgs

command :: ([Flag], [String], [String]) -> IO ()
command (opt, nonopt, err)
  | Help `elem` opt || null opt && null nonopt && null err = mapM_ putStrLn help
  | Version `elem` opt = putStrLn info
  | null err && not (null nonopt) = mapM_ (process opt) nonopt
  | otherwise = ioError (userError (concat err ++ usageInfo usage options))

process :: [Flag] -> String -> IO ()
process opt name
  = output
  . BS.toLazyByteString
  . renderJS
  =<< inject config (takeFileName name) <$> (lines <$> readFile name) <*> parseFile name
  where output | Print `elem` opt = BS.putStrLn
               | otherwise = BS.writeFile (replaceExtension name ".sjsp.js")
        config = Config { interval = get 10 getInterval isInterval opt
                        , top = get 20 getTop isTop opt
                        , accurate = Accurate `elem` opt
                        }
        get :: a -> (Flag -> a) -> (Flag -> Bool) -> [Flag] -> a
        get x f g = maybe x f . listToMaybe . reverse . filter g

options :: [OptDescr Flag]
options =
  [ Option "i"  ["interval"] (OptArg (Interval . fromMaybe "10") "INTERVAL")
                                             "interval time of logging the result in seconds (default 10)"
  , Option "t"  ["top"]      (OptArg (Top . fromMaybe "20") "TOP")
                                             "number of the results in the ranking (default 20)"
  , Option "a"  ["accurate"] (NoArg Accurate) "measure the time in accurate precision using performance.now() (default false)"
  , Option "p"  ["print"]    (NoArg Print)   "print out the compiled result to stdout"
  , Option "vV" ["version"]  (NoArg Version) "display the version number"
  , Option "h?" ["help"]     (NoArg Help)    "display this help"
  ]

info :: String
info = "sjsp " ++ showVersion version ++ "     Simple JavaScript Profiler"

usage :: String
usage = "Usage: sjsp [OPTIONS...] file\n"

help :: [String]
help = [ info
       , ""
       , usageInfo usage options
       , "Example:"
       , "  sjsp test.js     # generates test.sjsp.js"
       , ""
       , ""
       , "This software is released under the MIT License."
       , "This software is distributed at https://github.com/itchyny/sjsp."
       , "Report a bug of this software at https://github.com/itchyny/sjsp/issues."
       , "The author is itchyny <https://github.com/itchyny>."
       ]

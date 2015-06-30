module Main where

import Control.Applicative ((<$>), (<*>))
import Data.Version (showVersion)
import Language.JavaScript.Parser
import System.Environment (getArgs)
import System.FilePath.Posix (replaceExtension, takeFileName)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BS

import Injector (inject)
import Paths_sjsp (version)

main :: IO ()
main = getArgs >>= mainProgram

mainProgram :: [String] -> IO ()
mainProgram [] = printUsage
mainProgram args = mapM_ process args

process :: String -> IO ()
process name = BS.writeFile (replaceExtension name ".sjsp.js")
             . BS.toLazyByteString
             . renderJS
           =<< inject (takeFileName name) <$> (lines <$> readFile name) <*> parseFile name

printUsage :: IO ()
printUsage = mapM_ putStrLn usage

usage :: [String]
usage = [ "sjsp " ++ showVersion version ++ " Simple JavaScript Profiler"
        , ""
        , "Usage: sjsp [JavaScript files]"
        , ""
        , "Example:"
        , "  sjsp test.js     # generates test.sjsp.js"
        , ""
        , ""
        , "This software is released under the MIT License."
        , "This software is distributed at https://github.com/itchyny/sjsp."
        , "Report a bug of this software at https://github.com/itchyny/sjsp/issues."
        , "The author is itchyny <https://github.com/itchyny>."
        ]

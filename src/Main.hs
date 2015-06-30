module Main where

import Control.Applicative ((<$>), (<*>))
import Language.JavaScript.Parser
import System.Environment (getArgs)
import System.FilePath.Posix (replaceExtension, takeFileName)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BS

import Injector (inject)

process :: String -> IO ()
process name = BS.writeFile (replaceExtension name ".sjsp.js")
             . BS.toLazyByteString
             . renderJS
           =<< inject (takeFileName name) <$> (lines <$> readFile name) <*> parseFile name

mainProgram :: [String] -> IO ()
mainProgram = mapM_ process

main :: IO ()
main = getArgs >>= mainProgram

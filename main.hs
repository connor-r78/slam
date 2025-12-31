module Main ( main ) where

import Control.Monad
import Lexer
import Parser
import System.Environment
import System.IO

usage :: String -> String
usage progName = "Usage: " ++ progName ++ " <input file>"

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case args of
    [file] -> compile file
    _ -> do putStrLn $ usage progName

compile :: FilePath -> IO ()
compile file =
  withFile file ReadMode $ \handle -> do
      contents <- hGetContents handle
      let tokens = lexTokens (split contents)
      forM_ tokens (putStrLn . pretty)

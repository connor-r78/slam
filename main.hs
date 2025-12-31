module Main ( main ) where

import Parser
import System.Environment
import System.IO

usage :: String -> String
usage progName = "Usage: " ++ progName ++ " <input file>"

main = do
  args <- getArgs
  progName <- getProgName
  case args of
    [file] -> compile file
    _ -> do putStrLn $ usage progName

compile :: FilePath -> IO ()
compile file =
  withFile file ReadMode $ \handle -> do
    slm <- getSlm handle
    let ir = lexTokens slm
    mapM_ (putStrLn . pretty) ir

getSlm :: Handle -> IO [String]
getSlm h = hGetContents h >>= return . lines

module Main ( main ) where

import System.Environment
import System.IO

usage = do
  progName <- getProgName
  putStrLn ("Usage: " ++ progName ++ " <input file>")

main = do
  args <- getArgs
  case args of
    [file] -> compile file
    _ -> usage

compile :: FilePath -> IO ()
compile file =
  withFile file ReadMode $ \handle -> do
    slm <- getLines handle
    sequence_ $ map putStrLn slm

getLines :: Handle -> IO [String]
getLines h = hGetContents h >>= return . lines

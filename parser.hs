module Parser ( parse ) where

import Lexer

format :: String -> String
format str
  | comment   = ""
  | otherwise = str
  where
    comment = isComment str

parse :: [String] -> [String]
parse slm = filter (/= "") (map format slm)

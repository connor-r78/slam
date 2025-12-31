module Parser ( parse ) where

import Lexer

format :: String -> (String, Char)
format str
  | comment    = ("", 'c')
  | delimiter  = (str, 'd')
  | identifier = (str, 'i')
  | keyword    = (str, 'k')
  | operator   = (str, 'o')
  | whitespace = ("", 'w')
  | otherwise  = (str, ' ')
  where
    comment    = isComment str
    delimiter  = isDelimiter str
    identifier = isIdentifier str
    keyword    = isKeyword str
    operator   = isOperator str
    whitespace = isWhitespace str

parse :: [String] -> [String]
parse slm = filter (/= "") $ map (fst . format) slm

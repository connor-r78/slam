module Lexer ( 
  isComment,
  isDelimiter,
  isIdentifier,
  isKeyword,
  isOperator,
  isWhitespace
) where

import Data.List

isComment :: String -> Bool
isComment line = "//" `isInfixOf` line

isDelimiter :: String -> Bool
isDelimiter line = False

isIdentifier :: String -> Bool
isIdentifier line = True

isKeyword :: String -> Bool
isKeyword line = False

isOperator :: String -> Bool
isOperator line = False

isWhitespace :: String -> Bool
isWhitespace line = False

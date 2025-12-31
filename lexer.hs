module Lexer ( 
  isComment,
  isDelimiter,
  isIdentifier,
  isKeyword,
  isOperator,
  isWhitespace,
  split
) where

import Data.Char
import Data.List

split :: String -> [String]
split [] = []
split (c:cs)
  | isSpace c = split cs
  | isAlpha c =
      let (name, rest) = span isAlphaNum (c:cs)
      in name : split rest
  | isDigit c =
      let (num, rest) = span isDigit (c:cs)
      in num : split rest
  | otherwise =
      [c] : split cs

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

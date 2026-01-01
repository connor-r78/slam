module Lexer ( 
  isDelimiter,
  isIdentifier,
  isKeyword,
  isOperator,
  split
) where

import Data.Char
import Data.List

split :: String -> [String]
split [] = []
split s@(c:cs)
  | isSpace c = split cs
  | "//" `isPrefixOf` s =
    split (dropWhile (/= '\n') s)
  | isAlpha c =
    let (name, rest) = span isAlphaNum s
    in name : split rest
  | isDigit c =
    let (num, rest) = span isDigit s
    in num : split rest
  | Just (op, rest) <- matchOperator s =
    op : split rest
  | otherwise =
    [c] : split cs

operators :: [String]
operators =
  ["=", "::"]

sortedOperators :: [String]
sortedOperators =
  sortBy (\a b -> compare (length b) (length a)) operators

matchOperator :: String -> Maybe (String, String)
matchOperator input =
  case find (`isPrefixOf` input) sortedOperators of
    Just op -> Just (op, drop (length op) input)
    Nothing -> Nothing

isKeyword :: String -> Bool
isKeyword s = s `elem` ["expect",
                        "goal",
                        "limit",
                        "mod"]

isOperator :: String -> Bool
isOperator s = s `elem` operators

isDelimiter :: String -> Bool
isDelimiter s = s `elem` ["{", "}", "\""]

isIdentifier :: String -> Bool
isIdentifier [] = False
isIdentifier (c:cs) =
  isAlpha c && all isAlphaNum cs && not (isKeyword (c:cs))

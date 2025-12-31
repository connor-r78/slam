module Parser ( 
  lexTokens,
  pretty
) where

import Data.Maybe
import Lexer

data Token
  = TokKeyword String
  | TokIdent String
  | TokOperator String
  | TokDelimiter String
  deriving (Show)

pretty :: Token -> String
pretty (TokDelimiter str) = str
pretty (TokKeyword str)   = str
pretty (TokIdent str)     = str
pretty (TokOperator str)  = str

format :: String -> Maybe Token
format str
  | isComment str    = Nothing
  | isWhitespace str = Nothing
  | isIdentifier str = Just (TokIdent str)
  | isKeyword str    = Just (TokKeyword str)
  | isOperator str   = Just (TokOperator str)
  | isDelimiter str  = Just (TokDelimiter str)
  | otherwise        = Nothing

lexTokens :: [String] -> [Token]
lexTokens slm = mapMaybe format slm

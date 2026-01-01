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
  | isKeyword str    = Just (TokKeyword str)
  | isIdentifier str = Just (TokIdent str)
  | isOperator str   = Just (TokOperator str)
  | isDelimiter str  = Just (TokDelimiter str)
  | otherwise        = Nothing

lexTokens :: [String] -> [Token]
lexTokens slm = mapMaybe format slm

module Parser ( 
  createTree,
  lexTokens,
  pretty
) where

import Data.Maybe
import Lexer

type Parser a = [Token] -> Maybe (a, [Token])

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

data BinaryTree a
  = Empty
  | Node (BinaryTree a) a (BinaryTree a)
  | Leaf a
  deriving (Show)

treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
treeMap _ Empty = Empty
treeMap f (Leaf a) = Leaf (f a)
treeMap f (Node leftSubTree a rightSubTree) = Node (treeMap f leftSubTree) (f a) (treeMap f rightSubTree)

format :: String -> Maybe Token
format str
  | isKeyword str    = Just (TokKeyword str)
  | isIdentifier str = Just (TokIdent str)
  | isOperator str   = Just (TokOperator str)
  | isDelimiter str  = Just (TokDelimiter str)
  | otherwise        = Nothing

lexTokens :: [String] -> [Token]
lexTokens slm = mapMaybe format slm

insertInTree :: Token -> BinaryTree Token -> BinaryTree Token
insertInTree lexeme Empty = Leaf lexeme
insertInTree lexeme (Leaf x) = Node (Leaf lexeme) x Empty
insertInTree lexeme (Node left x right) = Node (insertInTree lexeme left) x right

createTree :: [Token] -> BinaryTree Token
createTree = foldl (flip insertInTree) Empty

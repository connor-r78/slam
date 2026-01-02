module Parser (
  Token(..),
  Tree(..),
  lexTokens,
  createTree,
  pretty,
  prettyTree
) where

import Data.Maybe (mapMaybe)

data Token
  = TokKeyword String
  | TokIdent String
  | TokOperator String
  | TokLiteral String
  | TokLBrace
  | TokRBrace 
  deriving (Eq, Show)

pretty :: Token -> String
pretty (TokKeyword s)  = s
pretty (TokIdent s)    = s
pretty (TokOperator s) = s
pretty (TokLiteral s)  = s

data Tree a
  = Tree a [Tree a]
  deriving (Eq, Show)

prettyTree :: Tree Token -> String
prettyTree = go ""
  where
    go prefix (Tree tok children) =
      prefix ++ pretty tok ++ "\n"
      ++ drawChildren prefix children

    drawChildren _ [] = ""
    drawChildren prefix [c] =
      go (prefix ++ "└── ") c
    drawChildren prefix (c:cs) =
      go (prefix ++ "├── ") c
      ++ drawChildren prefix cs

classify :: String -> Maybe Token
classify s = case s of
  "" -> Nothing
  _  -> if s == "{" then Just TokLBrace
        else if s == "}" then Just TokRBrace
        else if s `elem` ["mod","goal","expect","limit"] then Just (TokKeyword s)
        else if s `elem` ["=", "::"] then Just (TokOperator s)
        else if isQuoted s then Just (TokLiteral s)
        else if all (`elem` ['0'..'9']) s then Just (TokLiteral s)
        else Just (TokIdent s)
  where
    isQuoted :: String -> Bool
    isQuoted ('"':rest) = not (null rest) && last rest == '"'
    isQuoted _          = False

lexTokens :: [String] -> [Token]
lexTokens = mapMaybe classify

bindingPower :: Token -> Int
bindingPower (TokKeyword "mod")    = 1
bindingPower (TokKeyword "goal")   = 2
bindingPower (TokKeyword "expect") = 3
bindingPower (TokKeyword "limit")  = 4
bindingPower (TokOperator "=")     = 5
bindingPower (TokOperator "::")    = 6
bindingPower _                     = 0

insertInTree :: Token -> Tree Token -> Tree Token
insertInTree tok (Tree root children)
  | bindingPower tok > bindingPower root =
      case children of
        [] ->
          Tree root [Tree tok []]
        _  ->
          let (initChildren, lastChild) =
                (init children, last children)
          in Tree root (initChildren ++ [insertInTree tok lastChild])

  | otherwise =
      Tree root (children ++ [Tree tok []])

createTree :: [Token] -> Tree Token
createTree tokens = go [] Nothing tokens
  where
    go _ Nothing (t:ts) =
      go [] (Just (Tree t [])) ts
    go stack (Just current) [] =
      case stack of
        []     -> current
        (p:_)  -> p
    go stack (Just current) (TokLBrace:ts) =
      case current of
        Tree tok children ->
          case reverse children of
            [] -> go stack (Just current) ts
            (lastChild:rest) ->
              let parent =
                    Tree tok (reverse rest)
              in go (parent:stack) (Just lastChild) ts
    go (p:ps) (Just current) (TokRBrace:ts) =
      let merged =
            case p of
              Tree tok children ->
                Tree tok (children ++ [current])
      in go ps (Just merged) ts
    go stack (Just current) (tok:ts) =
      go stack (Just (insertInTree tok current)) ts

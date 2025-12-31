module Lexer ( isComment ) where

import Data.List

isComment :: String -> Bool
isComment line = "//" `isInfixOf` line

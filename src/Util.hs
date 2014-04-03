module Util where

import Types

reservedNames :: [String]
reservedNames = [ "rule"
                , "extend"
                , "def"
                , "escape"
                , "render-before"
                , "render-after"
                , "indent"
                , "include"
                ]

unquote :: String -> String
unquote ('\\':x:xs) = x : unquote xs
unquote ('"':xs)    = unquote xs
unquote (x:xs)      = x : unquote xs
unquote []          = []

isSpace :: Token -> Bool
isSpace (Space {}) = True
isSpace (LineBreak {}) = True
isSpace (Word {}) = False
isSpace (Quote {}) = False
isSpace (MultiQuote {}) = False
isSpace (ChangeRule {}) = False
isSpace (Bad {}) = False


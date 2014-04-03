{-# LANGUAGE NamedFieldPuns             #-}

module Util where

import           Control.Arrow
import           Control.Monad
import qualified Data.HashMap.Strict            as HashMap
import qualified Data.HashSet                   as HashSet
import qualified Data.List                      as List
import           Types

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
isSpace (Read {}) = False
isSpace (ChangeRule {}) = False
isSpace (Bad {}) = False

emptyRule :: Rule
emptyRule = Rule
            { ruleMacroMap = HashMap.empty
            , ruleAncestors = []
            , ruleEscapeMap = HashMap.empty
            , ruleRenderBefore = ""
            , ruleRenderAfter = ""
            , ruleIndent = ""
            }

emptyRendererState :: RendererState
emptyRendererState = RendererState
                     { _code = Document []
                     , _rendered = []
                     , _stack = []
                     , _ruleMap = HashMap.singleton [] emptyRule
                     , _workingRuleName = []
                     , _defRuleName = []
                     }

ancestors :: AbsoluteRuleName -> RuleMap -> [AbsoluteRuleName]
ancestors ruleName ruleMap
  | isCycle ruleName ruleMap = error "be cyclic"
  | otherwise = go ruleName ruleMap where
    go ruleName ruleMap = case HashMap.lookup ruleName ruleMap of
      Just (Rule {ruleAncestors}) -> join (map (\ancestor -> ancestor : go ancestor ruleMap) ruleAncestors)
      Nothing -> []

isCycle :: AbsoluteRuleName -> RuleMap -> Bool
isCycle = go HashSet.empty where
  go :: HashSet.HashSet AbsoluteRuleName -> AbsoluteRuleName -> RuleMap -> Bool
  go cycleGird ruleName ruleMap
    | HashSet.member ruleName cycleGird = True
    | otherwise = do
      case HashMap.lookup ruleName ruleMap of
        Nothing -> False
        Just (Rule {ruleAncestors}) -> do
          let newCycleGird = HashSet.union (HashSet.singleton ruleName) cycleGird
          let f ancestor = go newCycleGird ancestor ruleMap
          or (map f ruleAncestors)

renderedTextToString :: RenderedText -> String
renderedTextToString (RenderedLine line) = reverse ('\n':line)

renderedToString :: Rendered -> String
renderedToString = map renderedTextToString >>> reverse >>> join

unquoteMulti :: String -> String
unquoteMulti = dropWhile (=='"') >>> reverse >>> dropWhile (=='"') >>> reverse

tokenToString :: Token -> String
tokenToString (Word {tokenAsStr}) = tokenAsStr
tokenToString (Quote {tokenAsStr}) = unquote tokenAsStr
tokenToString (MultiQuote {tokenAsStr}) = unquoteMulti tokenAsStr
tokenToString (Space {tokenAsStr}) = tokenAsStr
tokenToString (LineBreak {tokenAsStr}) = tokenAsStr
-- TODO: otherwise

concatTokens :: [Token] -> String
concatTokens = map tokenToString >>> join

tokensToAbsoluteRuleName :: [Token] -> AbsoluteRuleName
tokensToAbsoluteRuleName tokens = reverse (reversedRec tokens) where
  reversedRec [] = []
  reversedRec tokens = do
    let name = takeWhile (not . isSpace) tokens
        rest = dropWhile isSpace (dropWhile (not . isSpace) tokens)
    concatTokens name : reversedRec rest

showAbsoluteRuleName :: AbsoluteRuleName -> String
showAbsoluteRuleName = reverse >>> List.intersperse " " >>> join
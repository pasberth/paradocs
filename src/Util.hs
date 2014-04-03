{-# LANGUAGE NamedFieldPuns             #-}

module Util where

import           Control.Arrow
import           Control.Monad
import qualified Data.List.Split                as Split
import qualified Data.HashMap.Strict            as HashMap
import qualified Data.HashSet                   as HashSet
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

isText :: Token -> Bool
isText (Word {}) = True
isText (Quote {}) = True
isText (MultiQuote {}) = True
isText _ = False

isBlank :: Token -> Bool
isBlank (Space {}) = True
isBlank (LineBreak {}) = True
isBlank _ = False

isRead :: Token -> Bool
isRead (Read {}) = True
isRead _ = False

isChangeRule :: Token -> Bool
isChangeRule (ChangeRule {}) = True
isChangeRule _ = False

isBad :: Token -> Bool
isBad (Bad {}) = True
isBad _ = False

rejectWhen :: (Token -> Bool) -> String -> Token -> Token
rejectWhen f errMsg tk
  | f tk = Bad { tokenAsStr = tokenAsStr tk, line = line tk, pos = pos tk, errMsg = errMsg }
  | otherwise = tk

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
tokensToAbsoluteRuleName = reverse . map concatTokens . Split.wordsBy isBlank

module Language.Paradocs.RuleEnv where

import           Control.Applicative
import           Control.Monad
import           Control.Lens
import qualified Data.HashSet                   as HashSet
import qualified Data.HashMap.Strict            as HashMap
import qualified Language.Paradocs.RuleName     as RuleName
import qualified Language.Paradocs.Rule         as Rule
import           Language.Paradocs.Rule(ancestors)

type RuleEnv = HashMap.HashMap RuleName.AbsoluteRuleName Rule.Rule

isCycle :: RuleName.AbsoluteRuleName -> RuleEnv -> Bool
isCycle = flip $ go HashSet.empty where
  go :: HashSet.HashSet RuleName.AbsoluteRuleName -> RuleEnv -> RuleName.AbsoluteRuleName -> Bool
  go cycleGird e ruleName
    | HashSet.member ruleName cycleGird = True
    | otherwise = do
      case e ^. at ruleName of
        Nothing -> False
        Just rule -> do
          let newCycleGird = HashSet.union (HashSet.singleton ruleName) cycleGird
          rule ^. ancestors . to (any (go newCycleGird e))

allAncestors :: RuleName.AbsoluteRuleName -> RuleEnv -> [RuleName.AbsoluteRuleName]
allAncestors ruleName e
  | isCycle ruleName e = error "be cyclic"
  | otherwise = go e ruleName where
    go e' ruleName' = case HashMap.lookup ruleName' e' of
      Just rule -> rule ^. ancestors . to (map ((:) <*> go e')) . to join
      Nothing -> []
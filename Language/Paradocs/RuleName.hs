module Language.Paradocs.RuleName where

type RelativeRuleName   = String
type AbsoluteRuleName   = [RelativeRuleName]

root :: AbsoluteRuleName
root = []
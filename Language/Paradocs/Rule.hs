{-# LANGUAGE TemplateHaskell            #-}

module Language.Paradocs.Rule where

import           Control.Lens
import qualified Data.HashMap.Strict            as HashMap
import qualified Language.Paradocs.File         as File
import qualified Language.Paradocs.RuleName     as RuleName
import qualified Language.Paradocs.MacroEnv     as MacroEnv
import qualified Language.Paradocs.EscapeEnv    as EscapeEnv

data Rule
  = Rule
    {
      _ancestors     :: [RuleName.AbsoluteRuleName],
      _macroEnv      :: MacroEnv.MacroEnv,
      _escapeEnv     :: EscapeEnv.EscapeEnv,
      _before        :: Maybe File.File,
      _after         :: Maybe File.File,
      _indent        :: Maybe String
    }
  deriving (Show)
makeLenses ''Rule

empty :: Rule
empty
  = Rule
    {
      _ancestors = [],
      _macroEnv = HashMap.empty,
      _escapeEnv = HashMap.empty,
      _before = Nothing,
      _after = Nothing,
      _indent = Nothing
    }

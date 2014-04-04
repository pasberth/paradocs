module Language.Paradocs.MacroEnv where

import qualified Language.Paradocs.File         as File
import qualified Data.HashMap.Strict            as HashMap

type MacroEnv = HashMap.HashMap String File.File
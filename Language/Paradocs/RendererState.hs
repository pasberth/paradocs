{-# LANGUAGE TemplateHaskell            #-}

module Language.Paradocs.RendererState where

import           Control.Monad
import           Control.Lens
import qualified Data.List.Split                as Split
import qualified Data.HashMap.Strict            as HashMap
import qualified Language.Paradocs.Rule         as Rule
import qualified Language.Paradocs.File         as File
import qualified Language.Paradocs.RuleName     as RuleName
import qualified Language.Paradocs.RuleEnv      as RuleEnv
import           Language.Paradocs.File(sourceCode, sourceLine, lineno, columnno)

data Rendered
  = Text String
  | Blank String
  | Structure [Rendered]
  | Failure String RendererState
  | Warning String RendererState
  | NoSuchFile FilePath RendererState
  deriving (Show)

data FileStackValue
  = FileStackValue
    {
      _stackRetFile :: File.File,
      _stackCallFile :: File.File
    }
  deriving (Show)

data RuleStackValue
  = RuleStackValue
    {
      _stackRuleName :: RuleName.AbsoluteRuleName,
      _stackStructure :: [Rendered]
    }
  deriving (Show)

data RendererState
  = RendererState
    {
      _ruleEnv :: RuleEnv.RuleEnv,
      _workingFile :: File.File,
      _workingRuleName :: RuleName.AbsoluteRuleName,
      _defRuleName :: RuleName.AbsoluteRuleName,
      _workingStructure :: [Rendered],
      _fileStack :: [FileStackValue],
      _ruleStack :: [RuleStackValue]
    }
  deriving (Show)

makeLenses ''FileStackValue
makeLenses ''RuleStackValue
makeLenses ''RendererState

empty :: RendererState
empty
  = RendererState
    {
      _ruleEnv = HashMap.singleton RuleName.root Rule.empty,
      _workingFile = File.empty,
      _workingRuleName = RuleName.root,
      _defRuleName = RuleName.root,
      _workingStructure = [],
      _fileStack = [],
      _ruleStack = []
    }

stepPosition :: String -> RendererState -> RendererState
stepPosition string renderState = do
  case reverse (Split.splitWhen (=='\n') string) of
    [] -> error "Language.Paradocs.stepPosition: error occurred"
    (_:[]) -> do
      renderState & workingFile . columnno +~ length string
    (lastLine:headLines) -> do
      let sourceCodeLines = renderState ^. workingFile . sourceCode & Split.splitWhen (=='\n')
      let no = renderState ^. workingFile . lineno
      let newSourceLine = sourceCodeLines !! (no + length headLines - 1)
      renderState & workingFile . lineno +~ length headLines
                  & workingFile . columnno .~ length lastLine + 1
                  & workingFile . sourceLine .~ newSourceLine

renderedToString :: Rendered -> String
renderedToString (Text s) = s
renderedToString (Structure st) = join $ map renderedToString st--dropSpaces where
  --dropSpaces = reverse $ dropWhile isBlank $ reverse $ dropWhile isBlank st
  --isBlank (Blank _) = True
  --isBlank _ = False
renderedToString (Warning _ _) = ""
renderedToString (Failure _ _) = ""
renderedToString (NoSuchFile _ _) = ""
renderedToString (Blank s) = s
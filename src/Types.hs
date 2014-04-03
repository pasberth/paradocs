{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types where

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.State
import           Control.Lens
import qualified Text.Trifecta                  as Trifecta
import qualified Text.Trifecta.Delta            as TrifectaDelta
import qualified Data.HashMap.Strict            as HashMap

data Token
  = Word            -- /\S+/
    { tokenAsStr    :: String
    , line          :: String
    , pos           :: TrifectaDelta.Delta
    }
  | Quote           -- /\\./, /".*?"/
    { tokenAsStr    :: String
    , line          :: String
    , pos           :: TrifectaDelta.Delta
    }
  | MultiQuote      -- /(""+).*?\1/m
    { tokenAsStr    :: String
    , line          :: String
    , pos           :: TrifectaDelta.Delta
    }
  | Space
    { tokenAsStr    :: String
    , line          :: String
    , pos           :: TrifectaDelta.Delta
    }
  | LineBreak
    { tokenAsStr    :: String
    , line          :: String
    , pos           :: TrifectaDelta.Delta
    }
  | Read            -- /%read/
    { tokenAsStr    :: String
    , line          :: String
    , pos           :: TrifectaDelta.Delta
    }
  | ChangeRule      -- /%\S+/
    { tokenAsStr    :: String
    , line          :: String
    , pos           :: TrifectaDelta.Delta
    }
  | Bad
    { tokenAsStr    :: String
    , line          :: String
    , pos           :: TrifectaDelta.Delta
    , errMsg        :: String
    }
  deriving (Show)

data Instr
  = DefRule                 -- %rule rule-token_1 rule-token_2 ... rule-token_n
  | DefExtend               -- %extend rule-token_1 rule-token_2 ... rule-token_n
  | DefMacro                -- %def identifier-token replacement-token_1 replacement-token_2 ... replacement-token_n
  | DefEscape               -- %escape identifier-token replacement-token_1 replacement-token_2 ... replacement-token_n
  | DefRenderBefore         -- %render-before render-token_1 render-token_2 ... render-token_n
  | DefRenderAfter          -- %render-after render-token_1 render-token_2 ... render-token_n
  | DefIndent               -- %indent indent-token_1 indent-token_2 ... indent-token_n
  | Include                 -- %include file-path-token_1 file-path-token_2 ... file-path-token_n
  deriving (Show)

data Stat
  = Paragraph [Token]
  | Instr Instr [Token]
  deriving (Show)

data AST = Document [Stat]
  deriving (Show)

data RenderedText
  = RenderedLine String
  deriving (Show)

type Rendered = [RenderedText]

type MacroMap = HashMap.HashMap String [Token]

type EscapeMap = HashMap.HashMap Char String

data Rule
  = Rule
    { ruleMacroMap      :: MacroMap
    , ruleAncestors     :: [AbsoluteRuleName]
    , ruleEscapeMap     :: EscapeMap
    , ruleRenderBefore  :: String
    , ruleRenderAfter   :: String
    , ruleIndent        :: String
    }
    deriving (Show)


type RelativeRuleName = String
type AbsoluteRuleName = [RelativeRuleName]

type RuleMap = HashMap.HashMap AbsoluteRuleName Rule

data StackValue
  = StackValue
    { stackWorkingRuleName :: AbsoluteRuleName
    , stackRendered :: Rendered
    }

type Stack = [StackValue]

data RendererState
  = RendererState
    { _code :: AST
    , _rendered :: Rendered
    , _stack :: Stack
    , _ruleMap :: RuleMap
    , _workingRuleName :: AbsoluteRuleName
    , _defRuleName :: AbsoluteRuleName
    }
makeLenses ''RendererState

newtype Renderer a
  = Renderer
    {
      unRenderer :: StateT RendererState IO a
    }
  deriving (Functor
            , Applicative
            , Monad
            , MonadState RendererState
            , MonadIO
            )

data ParserState
  = ParserState

newtype Parser a
  = Parser
    {
      unParser :: StateT ParserState Trifecta.Parser a
    }
  deriving (Functor
            , Applicative
            , Alternative
            , Monad
            , MonadPlus
            , MonadState ParserState
            , Trifecta.Parsing
            , Trifecta.DeltaParsing
            , Trifecta.CharParsing
            , Trifecta.TokenParsing
            )

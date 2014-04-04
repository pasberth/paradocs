{-# LANGUAGE TemplateHaskell            #-}

module Language.Paradocs.File where

import           Control.Lens
import qualified Language.Paradocs.Token        as Token

data File
  = File
    {
      _sourceCode :: String,
      _sourceLine :: String,
      _sourceToken :: String,
      _sourcePath :: Maybe FilePath,
      _sourceTokens :: [Token.Token String],
      _lineno :: Int,
      _columnno :: Int
    }
  deriving (Show)
makeLenses ''File

empty :: File
empty
  = File
    {
      _sourceCode = "",
      _sourceLine = "",
      _sourceToken = "",
      _sourcePath = Nothing,
      _sourceTokens = [],
      _lineno = 1,
      _columnno = 1
    }

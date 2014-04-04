{-# LANGUAGE LambdaCase #-}

import           Language.Paradocs
import qualified Language.Paradocs.RendererState as RendererState
import qualified Text.PrettyPrint.ANSI.Leijen   as Leijen
import qualified System.IO                      as System
import qualified System.Environment             as Environment

main = do
  Environment.getArgs >>= \case
    (sourcePath:[]) -> do
      renderFromFile sourcePath >>= \case
        Just rendered -> do
          printASTErrors rendered
          putStr $ RendererState.renderedToString rendered
        Nothing -> do
          Leijen.hPutDoc System.stderr (docError Leijen.<> boldString ("no such file `" ++ sourcePath ++ "'") Leijen.<> Leijen.linebreak)
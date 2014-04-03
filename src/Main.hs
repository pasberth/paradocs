{-# LANGUAGE LambdaCase                 #-}

module Main where

import           Control.Applicative
import           Control.Monad.Trans
import qualified Data.Monoid                    as Monoid
import qualified Data.ByteString.UTF8           as ByteString
import qualified Text.Trifecta                  as Trifecta
import qualified System.Exit                    as Exit
import qualified System.Environment             as Environment
import qualified Text.PrettyPrint.ANSI.Leijen   as Leijen
import qualified System.IO                      as System
import qualified System.Directory               as Directory
import           Types
import           Util
import           PrettyPrint
import           Parser
import           Renderer

main :: IO ()
main = do
  Environment.getArgs >>= \case
    [] -> do
      s <- getContents
      case Trifecta.parseString (runParser (document <* Trifecta.eof) ParserState) Monoid.mempty s of
        Trifecta.Failure _ -> Exit.exitFailure
        Trifecta.Success ast -> do
          s <- runRenderer render (emptyRendererState { _code = ast})
          putStr s
    (sourceFileName:[]) -> do
      exists <- Directory.doesFileExist sourceFileName

      if exists
        then do
          Trifecta.parseFromFile (runParser (document <* Trifecta.eof) ParserState) sourceFileName >>= \case
            Nothing -> Exit.exitFailure
            Just ast -> do
              s <- runRenderer render (emptyRendererState { _code = ast})
              putStr s
        else
          liftIO $ Leijen.hPutDoc System.stderr (docError Leijen.<> Leijen.bold (Leijen.string ("no such file `" ++ sourceFileName ++ "'")))
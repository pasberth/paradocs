{-# LANGUAGE NamedFieldPuns             #-}

module PrettyPrint where

import qualified Data.ByteString.UTF8           as ByteString
import qualified Text.Trifecta.Delta            as TrifectaDelta
import qualified Text.PrettyPrint.ANSI.Leijen   as Leijen
import qualified System.IO                      as System
import           Types

docDelta :: TrifectaDelta.Delta -> Leijen.Doc
docDelta (TrifectaDelta.Directed path lineno columnno _ _) = do
  let path_str = ByteString.toString path
  let path_doc = Leijen.bold (Leijen.string (path_str ++ ":"))
  let lineno_doc = Leijen.bold (Leijen.string (show (lineno + 1) ++ ":"))
  let columnno_doc = Leijen.bold (Leijen.string (show (columnno + 1) ++ ": "))
  path_doc Leijen.<> lineno_doc Leijen.<> columnno_doc
docDelta (TrifectaDelta.Columns columnno _) = do
  Leijen.bold (Leijen.string (show (columnno + 1) ++ ": "))
docDelta (TrifectaDelta.Lines lineno columnno _ _) = do
  let lineno_doc = Leijen.bold (Leijen.string (show (lineno + 1) ++ ":"))
  let columnno_doc = Leijen.bold (Leijen.string (show (columnno + 1) ++ ": "))
  lineno_doc Leijen.<> columnno_doc
docDelta _ = Leijen.string "???: "
-- TODO: otherwise

docError :: Leijen.Doc
docError = Leijen.bold (Leijen.red (Leijen.string "error: "))

docWarning :: Leijen.Doc
docWarning = Leijen.bold (Leijen.magenta (Leijen.string "warning: "))

docHintLine :: Token -> Leijen.Doc
docHintLine = go where
  go (Word {tokenAsStr, line, pos}) = impl tokenAsStr line pos
  go (Quote {tokenAsStr, line, pos}) = impl tokenAsStr line pos
  go (MultiQuote {tokenAsStr, line, pos}) = impl tokenAsStr line pos
  go (Space {tokenAsStr, line, pos}) = impl tokenAsStr line pos
  go (LineBreak {tokenAsStr, line, pos}) = impl tokenAsStr line pos
  go (ChangeRule {tokenAsStr, line, pos}) = impl tokenAsStr line pos
  go (Bad {tokenAsStr, line, pos}) = impl tokenAsStr line pos

  impl tokenAsStr line (TrifectaDelta.Directed _ _ columnno _ _) = do
    let (preToken, rest) = splitAt (fromInteger (toInteger columnno)) line
    let (tokenInLine, postToken) = if length tokenAsStr < length rest
                                      then splitAt (length tokenAsStr) rest
                                      else (rest, "")
    Leijen.string preToken Leijen.<> Leijen.underline (Leijen.string tokenInLine) Leijen.<> Leijen.string postToken
  impl tokenAsStr line (TrifectaDelta.Columns columnno _) = do
    let (preToken, rest) = splitAt (fromInteger (toInteger columnno)) line
    let (tokenInLine, postToken) = if length tokenAsStr < length rest
                                      then splitAt (length tokenAsStr) rest
                                      else (rest, "")
    Leijen.string preToken Leijen.<> Leijen.underline (Leijen.string tokenInLine) Leijen.<> Leijen.string postToken
  impl tokenAsStr line (TrifectaDelta.Lines _ columnno _ _) = do
    let (preToken, rest) = splitAt (fromInteger (toInteger columnno)) line
    let (tokenInLine, postToken) = if length tokenAsStr < length rest
                                      then splitAt (length tokenAsStr) rest
                                      else (rest, "")
    Leijen.string preToken Leijen.<> Leijen.underline (Leijen.string tokenInLine) Leijen.<> Leijen.string postToken

printTokenError :: Token -> String -> IO ()
printTokenError tk msg = do
  Leijen.hPutDoc System.stderr (docDelta (pos tk) Leijen.<> docError)
  Leijen.hPutDoc System.stderr (Leijen.bold (Leijen.string msg))
  Leijen.hPutDoc System.stderr Leijen.linebreak
  Leijen.hPutDoc System.stderr (docHintLine tk)

printTokenWarning :: Token -> String -> IO ()
printTokenWarning tk msg = do
  Leijen.hPutDoc System.stderr (docDelta (pos tk) Leijen.<> docWarning)
  Leijen.hPutDoc System.stderr (Leijen.bold (Leijen.string msg))
  Leijen.hPutDoc System.stderr Leijen.linebreak
  Leijen.hPutDoc System.stderr (docHintLine tk)


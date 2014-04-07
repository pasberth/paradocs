module Language.Paradocs.PrettyPrint where

import           Control.Lens
import qualified Text.PrettyPrint.ANSI.Leijen   as Leijen
import qualified System.IO                      as System
import           Language.Paradocs.Types

boldString :: String -> Leijen.Doc
boldString = Leijen.bold . Leijen.string

docError :: Leijen.Doc
docError = Leijen.red (boldString "error: ")

docWarning :: Leijen.Doc
docWarning = Leijen.magenta (boldString "warning: ")

docPosition :: File -> Leijen.Doc
docPosition file = do
  let path = maybe "" (++":") (file ^. sourcePath)
  let linno = show (file ^. lineno) ++ ":"
  let colno = show (file ^. columnno) ++ ": "
  boldString (path ++ linno ++ colno)

docHintLine :: File -> Leijen.Doc
docHintLine file = do
  let (preToken, rest) = splitAt (file ^. columnno - 1) (file ^. sourceLine)
  let tokenLen = length (file ^. sourceToken)
  let (tokenInLine, postToken) = if tokenLen < length rest
                                    then splitAt tokenLen rest
                                    else (rest, "")
  Leijen.string preToken Leijen.<> Leijen.underline (Leijen.string tokenInLine) Leijen.<> Leijen.string postToken

docMessage :: String -> Leijen.Doc -> RendererState -> Leijen.Doc
docMessage msgStr label renderState = do
  let pos = docPosition (renderState ^. workingFile)
  let msg = boldString msgStr
  let hintLine = docHintLine (renderState ^. workingFile)
  let traceFiles = renderState ^.. fileStack . each . stackCallFile
  let traceDocs = map (\file -> Leijen.string "\t" Leijen.<>  docPosition file Leijen.<> docHintLine file Leijen.<> Leijen.linebreak) traceFiles
  let stacktrace = foldl (Leijen.<>) (Leijen.string "") traceDocs
  let err = pos Leijen.<> label Leijen.<> msg Leijen.<> Leijen.linebreak Leijen.<> hintLine Leijen.<> Leijen.linebreak
  err Leijen.<> Leijen.linebreak Leijen.<> stacktrace

printASTErrors :: Rendered -> IO ()
printASTErrors (Text _) = return ()
printASTErrors (Blank _) = return ()
printASTErrors (Structure xs) = mapM_ printASTErrors xs
printASTErrors (Failure msgStr renderState) = do
  Leijen.hPutDoc System.stderr $ docMessage msgStr docError renderState
printASTErrors (Warning msgStr renderState) = do
  Leijen.hPutDoc System.stderr $ docMessage msgStr docWarning renderState
printASTErrors (NoSuchFile path renderState) = do
  Leijen.hPutDoc System.stderr $ docMessage ("no such file `" ++ path ++ "'") docError renderState

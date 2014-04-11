{-# LANGUAGE JavaScriptFFI              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Types
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Control.Lens
import           Language.Paradocs
import           Language.Paradocs.Types
import           Language.Paradocs.RendererState
import           Language.Paradocs.MonadStorage
import qualified Data.IORef                       as IORef
import qualified Data.Maybe                       as Maybe
import qualified Data.Aeson                       as Aeson
import qualified Data.HashMap.Strict              as HashMap
import qualified Data.Text                        as Text

data DOM
data Editor

foreign import javascript unsafe "window.onload = $1" onload :: (JSFun (IO a)) -> IO ()
foreign import javascript unsafe "CodeMirror.fromTextArea(document.getElementById('input'), { lineNumbers: true, lineWrapping: true })" initEditor :: IO (JSRef Editor)
foreign import javascript unsafe "document.getElementById($1)" getElementById :: JSString -> IO (JSRef DOM)
foreign import javascript unsafe "$2.onclick = $1" onclick :: (JSFun (IO a)) -> (JSRef DOM) -> IO ()
foreign import javascript unsafe "$1.doc.getValue()" getValue :: (JSRef Editor) -> IO JSString
foreign import javascript unsafe "var req=new XMLHttpRequest();req.open('GET',$1,false);req.send();$r = req.responseText;" get :: JSString -> IO JSString
foreign import javascript unsafe "$2.innerHTML = $1" setInnerHTML :: JSString -> (JSRef DOM) -> IO ()
foreign import javascript unsafe "JSON.parse($1)" parseJSON :: JSString -> JSRef Aeson.Value
foreign import javascript unsafe "NProgress.start()" startNProgress :: IO ()
foreign import javascript unsafe "NProgress.done()" doneNProgress :: IO ()

htmlError :: String
htmlError = "<span class='error'>error: </span>"

htmlWarning :: String
htmlWarning = "<span class='warning'>warning: </span>"

htmlPosition :: File -> String
htmlPosition file = do
  let path = maybe "" (++":") (file ^. sourcePath)
  let linno = show (file ^. lineno) ++ ":"
  let colno = show (file ^. columnno) ++ ": "
  "<span class='msg'>" ++ path ++ linno ++ colno ++ "</span>"

htmlHintLine :: File -> String
htmlHintLine file = do
  let (preToken, rest) = splitAt (file ^. columnno - 1) (file ^. sourceLine)
  let tokenLen = length (file ^. sourceToken)
  let (tokenInLine, postToken) = if tokenLen < length rest
                                    then splitAt tokenLen rest
                                    else (rest, "")
  preToken ++ "<u>" ++ tokenInLine ++ "</u>" ++ postToken

htmlMessage :: String -> String -> RendererState -> String
htmlMessage msg label renderState = do
  let pos = htmlPosition (renderState ^. workingFile)
  let hintLine = htmlHintLine (renderState ^. workingFile)
  let traceFiles = renderState ^.. fileStack . each . stackCallFile
  let traceDocs = map (\file -> "&nbsp;&nbsp;&nbsp;&nbsp;" ++ htmlPosition file ++ htmlHintLine file ++ "<br />") traceFiles
  let stacktrace = foldl (++) "" traceDocs
  let err = pos ++ label ++ "<span class='msg'>" ++ msg ++ "</span>" ++ "<br />" ++ hintLine ++ "<br />"
  err ++ "<br />" ++ stacktrace


htmlASTErrors :: Rendered -> String
htmlASTErrors (Text _) = ""
htmlASTErrors (Blank _) = ""
htmlASTErrors (Structure xs) = join $ map htmlASTErrors xs
htmlASTErrors (Failure msgStr renderState) = do
  htmlMessage msgStr htmlError renderState
htmlASTErrors (Warning msgStr renderState) = do
  htmlMessage msgStr htmlWarning renderState
htmlASTErrors (NoSuchFile path renderState) = do
  htmlMessage ("no such file `" ++ path ++ "'") htmlError renderState

main = do
  onloadCallback <- asyncCallback AlwaysRetain $ do
    startNProgress
    editor <- initEditor
    button <- getElementById (toJSString "render")
    view <- getElementById (toJSString "view")

    maybeLib <- fromJSRef =<< parseJSON <$> get (toJSString "lib.json")
    let { libStorage = case maybeLib of
                          Just libJSON ->
                            case libJSON of
                              Aeson.Object libObj -> HashMap.fromList $ map (\(k, (Aeson.String v)) -> (Text.unpack k, Text.unpack v)) $ HashMap.toList libObj
                              _ -> HashMap.empty
                          _ -> HashMap.empty }

    onclickCallback <- asyncCallback AlwaysRetain $ do
      startNProgress
      val <- getValue editor
      let rendered = runHashMapStorage (renderString (fromJSString val)) libStorage
      let s = renderedToString rendered
      let err = htmlASTErrors rendered
      setInnerHTML (toJSString (err ++ s)) view
      doneNProgress

    onclick onclickCallback button
    doneNProgress
  onload onloadCallback
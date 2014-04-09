{-# LANGUAGE JavaScriptFFI              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Char
import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Types
import           Control.Applicative
import           Control.Monad
import           Control.Lens
import           Language.Paradocs
import           Language.Paradocs.Types
import           Language.Paradocs.RendererState
import           Language.Paradocs.MonadStorage
import           Data.IORef

data DOM
data Editor

foreign import javascript unsafe "window.onload = $1" onload :: (JSFun (IO a)) -> IO ()
foreign import javascript unsafe "CodeMirror.fromTextArea(document.getElementById('input'), { lineNumbers: true, lineWrapping: true })" initEditor :: IO (JSRef Editor)
foreign import javascript unsafe "document.getElementById($1)" getElementById :: JSString -> IO (JSRef DOM)
foreign import javascript unsafe "$2.onclick = $1" onclick :: (JSFun (IO a)) -> (JSRef DOM) -> IO ()
foreign import javascript unsafe "$1.doc.getValue()" getValue :: (JSRef Editor) -> IO JSString
foreign import javascript unsafe "var req=new XMLHttpRequest();req.open('GET',$1);try{req.send();$r=true} catch (e){$r=false;}" available :: JSString -> IO JSBool
foreign import javascript unsafe "var req=new XMLHttpRequest();req.open('GET',$1,false);req.send();$r = req.responseText;" get :: JSString -> IO JSString
foreign import javascript unsafe "$2.innerHTML = $1" setInnerHTML :: JSString -> (JSRef DOM) -> IO ()

newtype JavaScriptStorage a
  = JavaScriptStorage (IO a)
  deriving (Functor, Applicative, Monad)

runJavaScriptStorage :: JavaScriptStorage a -> IO a
runJavaScriptStorage (JavaScriptStorage m) = m

instance MonadStorage JavaScriptStorage where
  maybeReadFile path = JavaScriptStorage $ do
    print path
    let jspath = toJSString path
    cond <- fromJSBool <$> available jspath
    if  cond
      then do
        Just . fromJSString <$> get jspath
      else do
        return Nothing

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
  onloadCallback <- syncCallback AlwaysRetain True $ do
    isFirst <- newIORef True
    editor <- initEditor
    button <- getElementById (toJSString "render")
    view <- getElementById (toJSString "view")
    onclickCallback <- syncCallback AlwaysRetain True $ do
      isFirst' <- readIORef isFirst
      if isFirst'
        then do
          writeIORef isFirst False
          setInnerHTML (toJSString "Please wait 2 seconds :)") view
        else do
          setInnerHTML (toJSString "少女 rendering...") view
      val <- getValue editor
      rendered <- runJavaScriptStorage $ renderString $ fromJSString val
      let s = renderedToString rendered
      let err = htmlASTErrors rendered
      setInnerHTML (toJSString (err ++ s)) view

    onclick onclickCallback button
  onload onloadCallback
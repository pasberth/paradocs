{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Main where

import           Control.Arrow
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.State
import           Control.Lens
import           Control.Monad.Trans.Loop
import qualified Data.Monoid                    as Monoid
import qualified Data.List                      as List
import qualified Data.List.Split                as Split
import qualified Data.Maybe                     as Maybe
import qualified Data.HashMap.Strict            as HashMap
import qualified Data.HashSet                   as HashSet
import qualified Data.ByteString.UTF8           as ByteString
import qualified Text.Trifecta                  as Trifecta
import qualified Text.Trifecta.Delta            as TrifectaDelta
import qualified System.IO                      as System
import qualified System.Exit                    as Exit
import qualified System.Directory               as Directory
import qualified System.FilePath.Posix          as FilePath
import qualified System.Environment             as Environment
import qualified Text.PrettyPrint.ANSI.Leijen   as Leijen
import           Types
import           Util
import           Parser

emptyRule :: Rule
emptyRule = Rule
            { ruleMacroMap = HashMap.empty
            , ruleAncestors = []
            , ruleEscapeMap = HashMap.empty
            , ruleRenderBefore = ""
            , ruleRenderAfter = ""
            , ruleIndent = ""
            }

emptyRendererState :: RendererState
emptyRendererState = RendererState
                     { _code = Document []
                     , _rendered = []
                     , _stack = []
                     , _ruleMap = HashMap.singleton [] emptyRule
                     , _workingRuleName = []
                     , _defRuleName = []
                     }

runRenderer :: Renderer a -> RendererState -> IO a
runRenderer = evalStateT . unRenderer

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

getWorkingRule :: Renderer Rule
getWorkingRule = do
  workingRuleName <- use workingRuleName
  ruleMap <- use ruleMap
  case HashMap.lookup workingRuleName ruleMap of
    Just rule -> return rule
    Nothing -> undefined

lookupAbsoluteRuleName :: RelativeRuleName -> Renderer (Maybe AbsoluteRuleName)
lookupAbsoluteRuleName relativeRuleName = do
  ruleMap <- use ruleMap
  lookupName (\name -> if HashMap.member (relativeRuleName:name) ruleMap then Just (relativeRuleName:name) else Nothing)

relativeRuleExists :: RelativeRuleName -> Renderer Bool
relativeRuleExists relativeRuleName = do
  lookupAbsoluteRuleName relativeRuleName >>= \case
    Just _ -> return True
    Nothing -> return False

lookupName :: (AbsoluteRuleName -> Maybe a) -> Renderer (Maybe a)
lookupName f = do
  path <- searchPath
  return $ foldl (\result rule -> result <|> f rule) Nothing path

lookupRule :: (Rule -> Maybe a) -> Renderer (Maybe a)
lookupRule f = do
  ruleMap <- use ruleMap
  lookupName (\name -> case HashMap.lookup name ruleMap of
                        Just rule -> f rule
                        Nothing -> Nothing)

searchPath :: Renderer [AbsoluteRuleName]
searchPath = do
    ruleName <- use workingRuleName
    ruleNames <- uses stack (map stackWorkingRuleName)
    ruleMap <- use ruleMap
    return (join (map (\name -> name : ancestors name ruleMap) (ruleName:ruleNames)))

ancestors :: AbsoluteRuleName -> RuleMap -> [AbsoluteRuleName]
ancestors ruleName ruleMap
  | isCycle ruleName ruleMap = error "be cyclic"
  | otherwise = go ruleName ruleMap where
    go ruleName ruleMap = case HashMap.lookup ruleName ruleMap of
      Just (Rule {ruleAncestors}) -> join (map (\ancestor -> ancestor : go ancestor ruleMap) ruleAncestors)
      Nothing -> []

isCycle :: AbsoluteRuleName -> RuleMap -> Bool
isCycle = go HashSet.empty where
  go :: HashSet.HashSet AbsoluteRuleName -> AbsoluteRuleName -> RuleMap -> Bool
  go cycleGird ruleName ruleMap
    | HashSet.member ruleName cycleGird = True
    | otherwise = do
      case HashMap.lookup ruleName ruleMap of
        Nothing -> False
        Just (Rule {ruleAncestors}) -> do
          let newCycleGird = HashSet.union (HashSet.singleton ruleName) cycleGird
          let f ancestor = go newCycleGird ancestor ruleMap
          or (map f ruleAncestors)

openAbsoluteRule :: AbsoluteRuleName -> Renderer ()
openAbsoluteRule absoluteRuleName = do
  wrn <- use workingRuleName
  tmp <- use rendered
  stack %= (StackValue { stackWorkingRuleName = wrn , stackRendered = tmp }:)
  rendered .= []
  workingRuleName .= absoluteRuleName

appendString :: String -> Renderer ()
appendString s = do
  rendered %= \case
                [] -> [RenderedLine (reverse s)]
                (RenderedLine line : rendered) -> RenderedLine (reverse s ++ line) : rendered

appendLine :: Renderer ()
appendLine = do
  rendered %= (RenderedLine [] :)

shouldIndent :: Renderer Bool
shouldIndent = uses rendered $ \case
                                [] -> True
                                (RenderedLine []:_) -> True
                                _ -> False

closeWorkingRule :: Renderer ()
closeWorkingRule = do
  Rule {ruleRenderBefore, ruleRenderAfter, ruleIndent} <- getWorkingRule
  StackValue { stackWorkingRuleName, stackRendered } <- uses stack head
  stack %= tail
  tmp <- use rendered
  workingRuleName .= stackWorkingRuleName 
  rendered .= stackRendered

  case Split.splitOn "\n" ruleRenderBefore of
    [] -> return ()
    (first:rest) -> do
      appendString first
      forM_ rest $ \line -> do
        appendLine
        appendString line

  let (linePrefix : headLines) = reverse (Split.splitOn "\n" ruleIndent)

  case reverse tmp of
    [] -> return ()
    (RenderedLine first : rest) -> do
        shouldIndent >>= \case
                          True -> appendString linePrefix
                          False -> return ()
        appendString (reverse first)
        forM_ rest $ \(RenderedLine line) -> do
          appendLine
          appendString (linePrefix ++ reverse line)

  case Split.splitOn "\n" ruleRenderAfter of
    [] -> return ()
    (first:rest) -> do
      appendString first
      forM_ rest $ \line -> do
        appendLine
        appendString line

renderChar :: Char -> Renderer ()
renderChar ch = do
  lookupRule (\Rule {ruleEscapeMap} -> HashMap.lookup ch ruleEscapeMap) >>= \case
    Just escapedCharacter
      -> do
        case Split.splitOn "\n" escapedCharacter of
          (first : rest) -> do
            appendString first
            forM_ rest $ \a -> do
              appendLine
              appendString a
    Nothing
      -> rendered %= \case
                      (RenderedLine line : rendered) -> RenderedLine (ch:line) : rendered
                      [] -> [RenderedLine [ch]]


execChangeRule :: RelativeRuleName -> Renderer ()
execChangeRule relativeRuleName = do
  relativeRuleExists relativeRuleName >>= \case
    True -> do

      wrname <- use workingRuleName
      wrnames <- uses stack (map stackWorkingRuleName)
      ruleMap <- use ruleMap

      foreach (wrname:wrnames) $ \ruleName -> do
        let absoluteRuleName = relativeRuleName : ruleName

        if HashMap.member absoluteRuleName ruleMap
          then do
            lift $ openAbsoluteRule absoluteRuleName
            exit
          else do
            forM_ (ancestors ruleName ruleMap) $ \ancestor -> do
              if HashMap.member (relativeRuleName : ancestor) ruleMap
                then do
                  lift $ openAbsoluteRule (relativeRuleName : ancestor)
                  exit
                else return ()
            lift $ closeWorkingRule
    False -> return ()

renderToken :: Token -> Renderer ()

renderToken (Word {tokenAsStr, line, pos}) = do
  lookupRule (\Rule {ruleMacroMap} -> HashMap.lookup tokenAsStr ruleMacroMap) >>= \case
    Just replacement -> mapM_ renderToken replacement
    Nothing -> do
      forM_ tokenAsStr $ \ch -> do
        lookupRule (\Rule {ruleMacroMap} -> HashMap.lookup [ch] ruleMacroMap) >>= \case
          Just replacement -> mapM_ renderToken replacement
          Nothing -> renderChar ch

renderToken (Quote {tokenAsStr}) = do
  forM_ (unquote tokenAsStr) $ \ch -> do
    renderChar ch

renderToken (MultiQuote {tokenAsStr}) = do
  forM_ (unquoteMulti tokenAsStr) $ \ch -> do
    renderChar ch

renderToken (Space {tokenAsStr}) = do
  appendString " "

renderToken (LineBreak {tokenAsStr}) = do
  rendered %= (RenderedLine "":)

renderToken tk@(ChangeRule { tokenAsStr, line, pos }) = do
  case tail tokenAsStr of 
    ".." -> do
      closeWorkingRule
    relativeRuleName -> do

      relativeRuleExists relativeRuleName >>= \case
        True -> execChangeRule relativeRuleName
        False -> do
          liftIO $ do
            Leijen.hPutDoc System.stderr (docDelta pos Leijen.<> docError)
            Leijen.hPutDoc System.stderr (Leijen.bold (Leijen.string ("not in scope `" ++ relativeRuleName ++ "'")))
            Leijen.hPutDoc System.stderr Leijen.linebreak
            Leijen.hPutDoc System.stderr (docHintLine tk)

renderToken tk@(Bad { tokenAsStr, line, pos, errMsg }) = do
  liftIO $ do
    Leijen.hPutDoc System.stderr (docDelta pos Leijen.<> docError)
    Leijen.hPutDoc System.stderr (Leijen.bold (Leijen.string errMsg))
    Leijen.hPutDoc System.stderr Leijen.linebreak
    Leijen.hPutDoc System.stderr (docHintLine tk)

  return ()

execInstr :: Instr -> [Token] -> Renderer ()

execInstr DefRule (_:tokens) = do
  targetRuleName <- tokensToAbsoluteRuleName <$> deleteBadTokens tokens
  uses ruleMap (HashMap.lookup targetRuleName) >>= \case
    Just _ -> do
      defRuleName .= targetRuleName
    Nothing -> do
      defRuleName .= targetRuleName
      ruleMap %= HashMap.insert targetRuleName emptyRule

execInstr DefExtend (instrName:tokens) = do
  extendingRuleName <- tokensToAbsoluteRuleName <$> deleteBadTokens tokens

  uses ruleMap (HashMap.member extendingRuleName) >>= \case
    True -> return ()
    False -> liftIO $ printTokenWarning instrName ("extending a not declared rule `" ++ join (List.intersperse " " (reverse extendingRuleName)) ++ "'")

  targetRuleName <- use defRuleName

  newRuleMap <- uses ruleMap (HashMap.adjust (\rule -> rule { ruleAncestors = extendingRuleName : ruleAncestors rule }) targetRuleName)

  if isCycle targetRuleName newRuleMap
    then liftIO $ printTokenError instrName "cyclic extend detected"
    else ruleMap .= newRuleMap

execInstr DefMacro (_:name:tokens_) = do
  let tokens = dropWhile isSpace tokens_
  rejectBadToken name >>= \case
    Just name -> do
      targetRuleName <- use defRuleName
      ruleMap %= HashMap.adjust (\rule -> rule { ruleMacroMap = HashMap.insert (tokenToString name) tokens (ruleMacroMap rule) }) targetRuleName
    Nothing -> return ()

execInstr DefEscape (_:name:tokens_) = do
  let tokens = dropWhile isSpace tokens_
  maybeEscapeTarget <- rejectBadToken name
  escapedString <- concatTokens <$> deleteBadTokens tokens

  case maybeEscapeTarget of
    Just escapeTarget -> do
      let (ch:_) = tokenToString escapeTarget
      targetRuleName <- use defRuleName
      ruleMap %= HashMap.adjust (\rule -> rule { ruleEscapeMap = HashMap.insert ch escapedString (ruleEscapeMap rule) }) targetRuleName
    Nothing -> return ()

execInstr DefRenderAfter (_:tokens) = do
  targetRuleName <- use defRuleName
  renderAfter <- concatTokens <$> deleteBadTokens tokens
  ruleMap %= HashMap.adjust (\rule -> rule { ruleRenderAfter = renderAfter }) targetRuleName

execInstr DefRenderBefore (_:tokens) = do
  targetRuleName <- use defRuleName
  renderBefore <- concatTokens <$> deleteBadTokens tokens
  ruleMap %= HashMap.adjust (\rule -> rule { ruleRenderBefore = renderBefore }) targetRuleName

execInstr DefIndent (_:tokens) = do
  indent <- concatTokens <$> deleteBadTokens tokens
  targetRuleName <- use defRuleName
  ruleMap %= HashMap.adjust (\rule -> rule { ruleIndent = indent }) targetRuleName

execInstr Include (instrName:tokens) = do
  filePath <- concatTokens <$> deleteBadTokens tokens

  case pos instrName of
    TrifectaDelta.Directed currentFilePath _ _ _ _ -> do
      let currentDirectoryPath = FilePath.dropFileName (ByteString.toString currentFilePath)
      let targetPath = FilePath.combine currentDirectoryPath filePath
      exists <- liftIO $ Directory.doesFileExist targetPath

      if exists
        then do
          liftIO (Trifecta.parseFromFile (runParser (document <* Trifecta.eof) ParserState) targetPath) >>= \case
            Just (Document codes1) ->
              code %= \case
                        Document codes2 -> Document (codes1 ++ codes2)
            Nothing -> return ()
        else
          liftIO $ printTokenError instrName ("no such file `" ++ targetPath ++ "'")

render :: Renderer String
render = use code >>= \case
  Document (Paragraph (token:tokens):codes)
    -> do code .= Document (Paragraph tokens:codes)
          renderToken token
          render
  Document (Paragraph []:codes)
    -> do code .= Document codes
          execChangeRule "paragraph"
          render
  Document (Instr instr tokens:codes)
    -> do code .= Document codes
          execInstr instr tokens
          render
  Document []
    -> do
      use stack >>= mapM_ (\_ -> closeWorkingRule)
      uses rendered renderedToString

renderedTextToString :: RenderedText -> String
renderedTextToString (RenderedLine line) = reverse ('\n':line)

renderedToString :: Rendered -> String
renderedToString = map renderedTextToString >>> reverse >>> join

unquoteMulti :: String -> String
unquoteMulti = dropWhile (=='"') >>> reverse >>> dropWhile (=='"') >>> reverse

rejectBadToken :: Token -> Renderer (Maybe Token)
rejectBadToken tk@(Bad {}) = do { renderToken tk ; return Nothing }
rejectBadToken tk = return $ Just tk

deleteBadTokens :: [Token] -> Renderer [Token]
deleteBadTokens tokens = Maybe.catMaybes <$> mapM rejectBadToken tokens

tokenToString :: Token -> String
tokenToString (Word {tokenAsStr}) = tokenAsStr
tokenToString (Quote {tokenAsStr}) = unquote tokenAsStr
tokenToString (MultiQuote {tokenAsStr}) = unquoteMulti tokenAsStr
tokenToString (Space {tokenAsStr}) = tokenAsStr
tokenToString (LineBreak {tokenAsStr}) = tokenAsStr
-- TODO: otherwise

concatTokens :: [Token] -> String
concatTokens = map tokenToString >>> join

tokensToAbsoluteRuleName :: [Token] -> AbsoluteRuleName
tokensToAbsoluteRuleName tokens = reverse (reversedRec tokens) where
  reversedRec [] = []
  reversedRec tokens = do
    let name = takeWhile (not . isSpace) tokens
        rest = dropWhile isSpace (dropWhile (not . isSpace) tokens)
    concatTokens name : reversedRec rest

showAbsoluteRuleName :: AbsoluteRuleName -> String
showAbsoluteRuleName = reverse >>> List.intersperse " " >>> join

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
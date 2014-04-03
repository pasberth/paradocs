{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Renderer where

import           Control.Applicative
import           Control.Monad.State
import           Control.Lens
import           Control.Monad.Trans.Loop
import qualified Data.Maybe                     as Maybe
import qualified Data.List                      as List
import qualified Data.List.Split                as Split
import qualified Data.HashMap.Strict            as HashMap
import qualified Data.Char                      as Char
import qualified Data.ByteString.UTF8           as ByteString
import qualified Text.Trifecta                  as Trifecta
import qualified Text.Trifecta.Delta            as TrifectaDelta
import qualified Text.PrettyPrint.ANSI.Leijen   as Leijen
import qualified System.IO                      as System
import qualified System.Directory               as Directory
import qualified System.FilePath.Posix          as FilePath
import           Types
import           Util
import           Parser
import           PrettyPrint

runRenderer :: Renderer a -> RendererState -> IO a
runRenderer = evalStateT . unRenderer

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
          let stripped = dropWhile Char.isSpace line
          if null stripped
            then return ()
            else do
              appendLine
              appendString (linePrefix ++ reverse stripped)

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

renderToken instrName@(Read { tokenAsStr, line, pos }) = do
  use code >>= \case
    -- TODO: 
    -- Document []
    Document (Paragraph tokens_:rest) -> do
      let tokens = dropWhile isBlank tokens_
      let tmp = map (rejectWhen (not . isText) "bad token") $ takeWhile (not . isBlank) tokens
      code .= Document (Paragraph (dropWhile (not . isBlank) tokens):rest)

      if or (map isBad tmp)
        then mapM_ renderToken tmp
        else do
          let filePath = concatTokens $ takeWhile (not . isBlank) tokens

          case pos of
            TrifectaDelta.Directed currentFilePath _ _ _ _ -> do
              let currentDirectoryPath = FilePath.dropFileName (ByteString.toString currentFilePath)
              let sourceFilePath = FilePath.combine currentDirectoryPath filePath
              exists <- liftIO $ Directory.doesFileExist sourceFilePath

              if exists
                then do
                  s <- liftIO $ readFile sourceFilePath
                  forM_ s $ \ch -> do
                    renderChar ch
                else
                  liftIO $ printTokenError instrName ("no such file `" ++ sourceFilePath ++ "'")

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
  code %= \case
            Document (Paragraph tokens:rest) -> Document (Paragraph (dropWhile isBlank tokens):rest)
            Document [] -> Document []

renderToken tk@(Bad { tokenAsStr, line, pos, errMsg }) = do
  liftIO $ do
    Leijen.hPutDoc System.stderr (docDelta pos Leijen.<> docError)
    Leijen.hPutDoc System.stderr (Leijen.bold (Leijen.string errMsg))
    Leijen.hPutDoc System.stderr Leijen.linebreak
    Leijen.hPutDoc System.stderr (docHintLine tk)

  return ()

execInstr :: Instr -> [Token] -> Renderer ()

execInstr DefRule (_:tokens) = do
  let tmp = map (rejectWhen (\tk -> not (isText tk || isBlank tk)) "bad token") tokens

  if or (map isBad tmp)
    then mapM_ renderToken (filter isBad tmp)
    else do
      let targetRuleName = tokensToAbsoluteRuleName tmp
      uses ruleMap (HashMap.lookup targetRuleName) >>= \case
        Just _ -> do
          defRuleName .= targetRuleName
        Nothing -> do
          defRuleName .= targetRuleName
          ruleMap %= HashMap.insert targetRuleName emptyRule

execInstr DefExtend (instrName:tokens) = do
  let tmp = map (rejectWhen (\tk -> not (isText tk || isBlank tk)) "bad token") tokens

  if or (map isBad tmp)
    then mapM_ renderToken (filter isBad tmp)
    else do
      let extendingRuleName = tokensToAbsoluteRuleName tmp

      uses ruleMap (HashMap.member extendingRuleName) >>= \case
        True -> return ()
        False -> liftIO $ printTokenWarning instrName ("extending a not declared rule `" ++ join (List.intersperse " " (reverse extendingRuleName)) ++ "'")

      targetRuleName <- use defRuleName

      newRuleMap <- uses ruleMap (HashMap.adjust (\rule -> rule { ruleAncestors = extendingRuleName : ruleAncestors rule }) targetRuleName)

      if isCycle targetRuleName newRuleMap
        then liftIO $ printTokenError instrName "cyclic extend detected"
        else ruleMap .= newRuleMap

execInstr DefMacro (instrName:tokens) = do
  case break isBlank tokens of
    ([], _) -> do
      liftIO $ printTokenError instrName "definition expected"
    (name, body) -> do
      let tmp = map (rejectWhen (not . isText) "bad token") name
      if or (map isBad tmp)
        then mapM_ renderToken (filter isBad tmp)
        else do
          targetRuleName <- use defRuleName
          ruleMap %= HashMap.adjust (\rule -> rule { ruleMacroMap = HashMap.insert (concatTokens name) (dropWhile isBlank body) (ruleMacroMap rule) }) targetRuleName

execInstr DefEscape (instrName:tokens) = do
  case break isBlank tokens of
    ([], _) -> do
      liftIO $ printTokenError instrName "definition expected"
    (name, body) -> do
      let tmp = map (rejectWhen (not . isText) "bad token") name
      if or (map isBad tmp)
        then mapM_ renderToken (filter isBad tmp)
        else do
          case concatTokens name of
            (ch:[]) -> do
              targetRuleName <- use defRuleName
              ruleMap %= HashMap.adjust (\rule -> rule { ruleEscapeMap = HashMap.insert ch (concatTokens (dropWhile isBlank body)) (ruleEscapeMap rule) }) targetRuleName
            s -> do
              liftIO $ printTokenError ((head name) { tokenAsStr = join (map tokenAsStr name) }) ("a character expected but got `" ++ s ++ "'")

execInstr DefRenderAfter (_:tokens) = do
  targetRuleName <- use defRuleName
  let tmp = map (rejectWhen (\tk -> not (isText tk || isBlank tk)) "bad token") tokens

  if or (map isBad tmp)
    then mapM_ renderToken (filter isBad tmp)
    else do
      let renderAfter = concatTokens tokens
      ruleMap %= HashMap.adjust (\rule -> rule { ruleRenderAfter = renderAfter }) targetRuleName

execInstr DefRenderBefore (_:tokens) = do
  targetRuleName <- use defRuleName

  let tmp = map (rejectWhen (\tk -> not (isText tk || isBlank tk)) "bad token") tokens

  if or (map isBad tmp)
    then mapM_ renderToken (filter isBad tmp)
    else do
      let renderBefore = concatTokens tokens
      ruleMap %= HashMap.adjust (\rule -> rule { ruleRenderBefore = renderBefore }) targetRuleName

execInstr DefIndent (_:tokens) = do
  targetRuleName <- use defRuleName

  let tmp = map (rejectWhen (\tk -> not (isText tk || isBlank tk)) "bad token") tokens

  if or (map isBad tmp)
    then mapM_ renderToken (filter isBad tmp)
    else do
      let indent = concatTokens tokens
      ruleMap %= HashMap.adjust (\rule -> rule { ruleIndent = indent }) targetRuleName

execInstr Include (instrName:tokens) = do
  case break isBlank tokens of
    (name, rest) -> do
      let tmp1 = map (rejectWhen (not . isBlank) "bad token") rest
      let tmp = map (rejectWhen (not . isText) "bad token") name
      if or (map isBad tmp1) || or (map isBad tmp)
        then do
          mapM_ renderToken (filter isBad tmp1)
          mapM_ renderToken (filter isBad tmp)
        else do
          let filePath = concatTokens name
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

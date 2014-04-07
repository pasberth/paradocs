{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE LambdaCase                 #-}

module Language.Paradocs.Renderer( RendererT(..)
                                 , Rendered(..)
                                 , render
                                 , renderFromFile
                                 , renderString
                                 ) where

import           Control.Arrow
import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Loop
import           Control.Lens
import           Control.Bool
import qualified Data.Char                      as Char
import qualified Data.HashMap.Strict            as HashMap
import           Language.Paradocs.Types
import qualified Language.Paradocs.File         as File
import qualified Language.Paradocs.Rule         as Rule
import qualified Language.Paradocs.RuleEnv      as RuleEnv
import qualified Language.Paradocs.RendererState as RendererState
import qualified Language.Paradocs.Token        as Token
import qualified Language.Paradocs.MonadStorage as MonadStorage
import qualified System.FilePath.Posix          as FilePath

newtype RendererT m a
  = RendererT
    {
      unRenderer :: StateT RendererState m a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadTrans
           , MonadState RendererState
           )

type Scope = [AbsoluteRuleName]

runRendererT :: Monad m => RendererT m a -> RendererState -> m a
runRendererT = evalStateT . unRenderer

appendRendered :: Monad m => Rendered -> RendererT m ()
appendRendered rendered = do
  workingStructure %= (rendered:)

appendString :: Monad m => String -> RendererT m ()
appendString str = do
  appendRendered $ Text str

appendChar :: Monad m => Char -> RendererT m ()
appendChar ch = do
  appendRendered $ Text [ch]

failure :: Monad m => String -> RendererT m ()
failure msg = do
  s <- get
  appendRendered $ Failure msg s

noSuchFile :: Monad m => FilePath -> RendererT m ()
noSuchFile path = do
  s <- get
  appendRendered $ NoSuchFile path s


relativeScope :: Monad m => RendererT m Scope
relativeScope = do
  e <- use ruleEnv
  ruleName <- use workingRuleName
  return $ ruleName : RuleEnv.allAncestors ruleName e

dynamicScope :: Monad m => RendererT m Scope
dynamicScope = do
  e <- use ruleEnv
  ruleName <- use workingRuleName
  ruleNames <- uses ruleStack (^.. each . stackRuleName)
  return $ (ruleName:ruleNames) & map (\name -> name : RuleEnv.allAncestors name e) & join

lookupNameIn :: Monad m => (AbsoluteRuleName -> Maybe a) -> Scope -> RendererT m (Maybe a)
lookupNameIn f scope = return $ foldl (\result rule -> result <|> f rule) Nothing scope

--lookupNameInDynamicScope :: Monad m => (AbsoluteRuleName -> Maybe a) -> RendererT m (Maybe a)
--lookupNameInDynamicScope f = dynamicScope >>= lookupNameIn f

--lookupNameInRelativeScope :: Monad m => (AbsoluteRuleName -> Maybe a) -> RendererT m (Maybe a)
--lookupNameInRelativeScope f = relativeScope >>= lookupNameIn f

lookupRuleIn :: Monad m => (Rule -> Maybe a) -> Scope -> RendererT m (Maybe a)
lookupRuleIn f scope = do
  e <- use ruleEnv
  lookupNameIn (\name -> case HashMap.lookup name e of
                        Just rule -> f rule
                        Nothing -> Nothing) scope

lookupRuleInDynamicScope :: Monad m => (Rule -> Maybe a) -> RendererT m (Maybe a)
lookupRuleInDynamicScope f = dynamicScope >>= lookupRuleIn f

lookupAbsoluteRuleNameIn :: Monad m => RelativeRuleName -> Scope -> RendererT m (Maybe AbsoluteRuleName)
lookupAbsoluteRuleNameIn relativeRuleName scope = do
  e <- use ruleEnv
  lookupNameIn (\name -> if HashMap.member (relativeRuleName:name) e
                          then Just (relativeRuleName:name)
                          else Nothing) scope

--lookupAbsoluteRuleNameInDynamicScope :: Monad m => RelativeRuleName -> RendererT m (Maybe AbsoluteRuleName)
--lookupAbsoluteRuleNameInDynamicScope relativeRuleName = dynamicScope >>= lookupAbsoluteRuleNameIn relativeRuleName

lookupAbsoluteRuleNameInRelativeScope :: Monad m => RelativeRuleName -> RendererT m (Maybe AbsoluteRuleName)
lookupAbsoluteRuleNameInRelativeScope relativeRuleName = relativeScope >>= lookupAbsoluteRuleNameIn relativeRuleName

doesRuleExistIn :: Monad m => RelativeRuleName -> Scope -> RendererT m Bool
doesRuleExistIn relativeRuleName scope = do
  lookupAbsoluteRuleNameIn relativeRuleName scope >>= \case
    Just _ -> return True
    Nothing -> return False

doesRuleExistInDynamicScope :: Monad m => RelativeRuleName -> RendererT m Bool
doesRuleExistInDynamicScope relativeRuleName = dynamicScope >>= doesRuleExistIn relativeRuleName

--doesRuleExistInRelativeScope :: Monad m => RelativeRuleName -> RendererT m Bool
--doesRuleExistInRelativeScope relativeRuleName = relativeScope >>= doesRuleExistIn relativeRuleName

openFile :: Monad m => File -> RendererT m ()
openFile file = do
  ret <- use workingFile
  fileStack %= (FileStackValue ret ret :)
  workingFile .= file

openFileFrom :: Monad m => File -> File -> RendererT m ()
openFileFrom call file = do
  ret <- use workingFile
  fileStack %= (FileStackValue ret call :)
  workingFile .= file

openAbsoluteRule :: Monad m => AbsoluteRuleName -> RendererT m ()
openAbsoluteRule absoluteRuleName = do
  nm <- use workingRuleName
  st <- use workingStructure

  ruleStack %= (RuleStackValue nm st :)
  workingRuleName .= absoluteRuleName
  workingStructure .= []

  uses ruleEnv (^. at absoluteRuleName) >>= \case
    Just rule -> do
      case rule ^. before of
        Just bfrFile ->
          openFile bfrFile
        Nothing ->
          return ()
    Nothing ->
      return ()

closeFile :: Monad m => RendererT m ()
closeFile = do
  FileStackValue ret _ <- uses fileStack head
  fileStack %= tail
  workingFile .= ret

closeWorkingRule :: Monad m => RendererT m ()
closeWorkingRule = do
  absoluteRuleName <- use workingRuleName

  st1 <- use workingStructure
  RuleStackValue nm st <- uses ruleStack head
  ruleStack %= tail
  workingRuleName .= nm
  workingStructure .= (Structure (reverse st1):st)

  uses ruleEnv (^. at absoluteRuleName) >>= \case
    Just rule -> do
      case rule ^. after of
        Just aftFile ->
          openFile aftFile
        Nothing ->
          return ()
    Nothing ->
      return ()

render :: MonadStorage m => RendererT m Rendered
render = do
  let rec :: MonadStorage m => RendererT m ()
      rec = use (workingFile . sourceTokens) >>=
              \case
                []      -> do
                  ifThenElseM (uses fileStack null)
                    (do
                      unlessM (uses ruleStack null) $ do
                        closeWorkingRule
                        rec)
                    (do
                      closeFile
                      rec)
                (_:_)  -> do { render1 ; rec }
  rec
  st <- use workingStructure
  return $ Structure (reverse st)

render1 :: MonadStorage m => RendererT m ()
render1 = use (workingFile . sourceTokens) >>= \case
  [] ->
    return ()
  (tk:_) -> case tk of
    Token RawBackslash _ ->
      renderRawQuote
    Token RawQuote1 _ ->
      renderRawQuote
    Token RawQuoteN _ ->
      renderRawQuote
    Token Instruction "%rule" ->
      renderRuleInstruction
    Token Instruction "%extend" ->
      renderExtendInstruction
    Token Instruction "%def" ->
      renderDefInstruction
    Token Instruction "%escape" ->
      renderEscapeInstruction
    Token Instruction "%before" ->
      renderBeforeInstruction
    Token Instruction "%after" ->
      renderAfterInstruction
    --Token Instruction "%indent" ->
    --  renderIndentInstruction
    Token Instruction "%include" ->
      renderIncludeInstruction
    Token Instruction "%read" ->
      renderReadInstruction
    Token Instruction _ ->
      renderInstruction
    Token Backslash _ ->
      renderQuote
    Token Quote1 _ ->
      renderQuote
    Token QuoteN _ ->
      renderQuote
    Token Word _ ->
      renderWord
    Token ParagraphBreak _ ->
      renderParagraphBreak
    Token LineBreak _ ->
      renderLineBreak
    Token Space _ ->
      renderSpace
    Token (Bad _) _ ->
      renderBad
    Empty ->
      undefined

renderText :: Monad m => RendererT m ()
renderText = do
  Token _ s <- uses (workingFile . sourceTokens) head
  dropToken
  appendRendered $ Blank s

renderRawQuote :: Monad m => RendererT m ()
renderRawQuote = do
  s <- uses (workingFile . sourceTokens) (head >>> Token.unquote)
  dropToken
  appendString s

renderRuleInstruction :: Monad m => RendererT m ()
renderRuleInstruction = do
    instructionToken <- uses (workingFile . sourceTokens) head
    ruleNameTokens <- uses (workingFile . sourceTokens) (tail >>> Token.takeParagraph)
    if Token.isValidAsRuleName ruleNameTokens
      then do
        let absoluteRuleName = Token.toAbsoluteRuleName ruleNameTokens
        defRuleName .= absoluteRuleName
        unlessM (uses ruleEnv (HashMap.member absoluteRuleName)) $
          ruleEnv %= HashMap.insert absoluteRuleName Rule.empty
        forM_ (instructionToken:ruleNameTokens) $ \_ -> dropToken
      else do
        workingFile . sourceToken .= Token.toString instructionToken
        failure "the rule wasn't defined because the following errors occurred"
        dropToken
        forM_ ruleNameTokens $ \tk ->
          if
            | Token.isBad tk    -> renderBad
            | Token.isBlank tk  -> dropToken
            | Token.isWord tk   -> dropToken
            | otherwise         -> do
              workingFile . sourceToken .= Token.toString tk
              failure "bad token"
              dropToken

renderExtendInstruction :: Monad m => RendererT m ()
renderExtendInstruction = do
    instructionToken <- uses (workingFile . sourceTokens) head
    ruleNameTokens <- uses (workingFile . sourceTokens) (tail >>> Token.takeParagraph)
    if Token.isValidAsRuleName ruleNameTokens
      then do
        targetRuleName <- use defRuleName
        let absoluteRuleName = Token.toAbsoluteRuleName ruleNameTokens
        newRuleEnv <- uses ruleEnv (HashMap.adjust (ancestors %~ (absoluteRuleName:)) targetRuleName)
        if RuleEnv.isCycle targetRuleName newRuleEnv
          then do
            workingFile . sourceToken .= Token.toString instructionToken
            failure "cyclic inheritance detected"
          else do
            ruleEnv .= newRuleEnv
        forM_ (instructionToken:ruleNameTokens) $ \_ -> dropToken
      else do
        workingFile . sourceToken .= Token.toString instructionToken
        failure "the inheritance wasn't applied because the following errors occurred"
        dropToken
        forM_ ruleNameTokens $ \tk ->
          if
            | Token.isBad tk    -> renderBad
            | Token.isBlank tk  -> dropToken
            | Token.isWord tk   -> dropToken
            | otherwise         -> do
              workingFile . sourceToken .= Token.toString tk
              failure "bad token"
              dropToken

renderDefInstruction :: Monad m => RendererT m ()
renderDefInstruction = do
  instructionToken <- uses (workingFile . sourceTokens) head
  (blank1, (name, (blank2, (blank3, body))))
      <- uses (workingFile . sourceTokens)
              (tail >>> Token.takeParagraph
                    >>> span Token.isBlank
                    >>> second (break Token.isBlank
                                >>> second (span Token.isBlank
                                            >>> second (reverse
                                                        >>> span Token.isBlank
                                                        >>> reverse *** reverse))))
  let nameStr = join $ map Token.unquote name

  if
    | null name -> do
      workingFile . sourceToken .= Token.toString instructionToken
      failure "identifier expected"
      dropToken
      forM_ (blank1 ++ name ++ blank2 ++ body ++ blank3) $ \_ -> dropToken
    | any Char.isSpace nameStr -> do
      forM_ (instructionToken:blank1) $ \_ -> dropToken
      workingFile . sourceToken .= join (map Token.toString name)
      failure "the identifier includes spaces"
      forM_ (name ++ blank2 ++ body ++ blank3) $ \_ -> dropToken
    | otherwise -> do
      dropToken
      renderState <- get
      let renderState' = renderState & RendererState.stepPosition (join (map Token.toString (join [blank1, name, blank2])))
                                  & workingFile . sourceTokens .~ body
      targetRuleName <- use defRuleName
      ruleEnv %= HashMap.adjust (macroEnv . at nameStr ?~ (renderState' ^. workingFile)) targetRuleName
      forM_ (blank1 ++ name ++ blank2 ++ body ++ blank3) $ \_ -> dropToken

renderEscapeInstruction :: Monad m => RendererT m ()
renderEscapeInstruction = do
  instructionToken <- uses (workingFile . sourceTokens) head
  (blank1, (name, (blank2, (blank3, body))))
      <- uses (workingFile . sourceTokens)
              (tail >>> Token.takeParagraph
                    >>> span Token.isBlank
                    >>> second (break Token.isBlank
                                >>> second (span Token.isBlank
                                            >>> second (reverse
                                                        >>> span Token.isBlank
                                                        >>> reverse *** reverse))))
  let nameStr = join $ map Token.unquote name

  if
    | null name -> do
      workingFile . sourceToken .= Token.toString instructionToken
      failure "identifier expected"
      dropToken
      forM_ (blank1 ++ name ++ blank2 ++ body ++ blank3) $ \_ -> dropToken
    | length nameStr /= 1 -> do
      forM_ (instructionToken:blank1) $ \_ -> dropToken
      workingFile . sourceToken .= join (map Token.toString name)
      failure $ "a character expected but got " ++ show (length nameStr) ++ " characters"
      forM_ (name ++ blank2 ++ body ++ blank3) $ \_ -> dropToken
    | any (not . Token.isRaw) body -> do
        workingFile . sourceToken .= Token.toString instructionToken
        failure "the definition of an escape wasn't applied because the following errors occurred"
        forM_ (instructionToken : (blank1 ++ name ++ blank2)) $ \_ -> dropToken
        forM_ body $ \tk ->
          if
            | Token.isBad tk    -> renderBad
            | Token.isRaw tk    -> dropToken
            | otherwise         -> do
              workingFile . sourceToken .= Token.toString tk
              failure "bad token"
              dropToken
        forM_ blank3 $ \_ -> dropToken
    | otherwise -> do
      let nameCh = head nameStr
      let bodyStr = join $ map Token.unquote body
      dropToken
      targetRuleName <- use defRuleName
      ruleEnv %= HashMap.adjust (escapeEnv . at nameCh ?~ bodyStr) targetRuleName
      forM_ (blank1 ++ name ++ blank2 ++ body ++ blank3) $ \_ -> dropToken

renderBeforeInstruction :: Monad m => RendererT m ()
renderBeforeInstruction = do
  instructionToken <- uses (workingFile . sourceTokens) head
  (blank, (blank1, body))
      <- uses (workingFile . sourceTokens)
              (tail >>> Token.takeParagraph
                    >>> span Token.isBlank
                    >>> second (reverse
                                >>> span Token.isBlank
                                >>> reverse *** reverse))
  forM_ (instructionToken:blank) $ \_ -> dropToken
  renderState <- get
  let renderState' = renderState & workingFile . sourceTokens .~ body
  targetRuleName <- use defRuleName
  ruleEnv %= HashMap.adjust (before ?~ (renderState' ^. workingFile)) targetRuleName
  forM_ (body ++ blank1) $ \_ -> dropToken

renderAfterInstruction :: Monad m => RendererT m ()
renderAfterInstruction = do
  instructionToken <- uses (workingFile . sourceTokens) head
  (blank, (blank1, body))
      <- uses (workingFile . sourceTokens)
              (tail >>> Token.takeParagraph
                    >>> span Token.isBlank
                    >>> second (reverse
                                >>> span Token.isBlank
                                >>> reverse *** reverse))
  forM_ (instructionToken:blank) $ \_ -> dropToken
  renderState <- get
  let renderState' = renderState & workingFile . sourceTokens .~ body
  targetRuleName <- use defRuleName
  ruleEnv %= HashMap.adjust (after ?~ (renderState' ^. workingFile)) targetRuleName
  forM_ (body ++ blank1) $ \_ -> dropToken

{-renderIndentInstruction :: Monad m => RendererT m ()
renderIndentInstruction = do
  instructionToken <- uses (workingFile . sourceTokens) head
  (blank, (blank1, body))
      <- uses (workingFile . sourceTokens)
              (tail >>> Token.takeParagraph
                    >>> span Token.isBlank
                    >>> second (reverse
                                >>> span Token.isBlank
                                >>> reverse *** reverse))
  if 
    | any (not . Token.isRaw) body -> do
        workingFile . sourceToken .= Token.toString instructionToken
        failure "the definition of indent wasn't applied because the following errors occurred"
        forM_ (instructionToken : blank) $ \_ -> dropToken
        forM_ body $ \tk ->
          if
            | Token.isBad tk    -> renderBad
            | Token.isRaw tk    -> dropToken
            | otherwise         -> do
              workingFile . sourceToken .= Token.toString tk
              failure "bad token"
              dropToken
        forM_ blank1 $ \_ -> dropToken
    | otherwise -> do
      let bodyStr = join $ map Token.unquote body
      dropToken
      targetRuleName <- use defRuleName
      ruleEnv %= HashMap.adjust (indent ?~ bodyStr) targetRuleName
      forM_ (blank ++ body ++ blank1) $ \_ -> dropToken-}

renderIncludeInstruction :: MonadStorage m => RendererT m ()
renderIncludeInstruction = do
  instructionToken <- uses (workingFile . sourceTokens) head
  (blank, relativePath)
      <- uses (workingFile . sourceTokens)
              (tail >>> Token.takeParagraph
                    >>> span Token.isBlank
                    >>> second (takeWhile (not . Token.isBlank)))
  let relativePathStr = join $ map Token.unquote relativePath
  directory <- use (workingFile . sourcePath) >>= return . maybe "./" FilePath.dropFileName
  let path = FilePath.normalise $ FilePath.combine directory relativePathStr

  forM_ (instructionToken:blank) $ \_ -> dropToken
  lift (MonadStorage.maybeReadFile path) >>= \case
    Just contents -> do
      let file = File.empty & sourceCode .~ contents
                            & sourceLine .~ head (lines contents)
                            & sourcePath ?~ path
                            & sourceTokens .~ Token.tokenize contents
      forM_ relativePath $ \_ -> dropToken
      openFile file
    Nothing -> do
      workingFile . sourceToken .= join (map Token.toString relativePath)
      noSuchFile path
      forM_ relativePath $ \_ -> dropToken

renderReadInstruction :: MonadStorage m => RendererT m ()
renderReadInstruction = do
  instructionToken <- uses (workingFile . sourceTokens) head
  (blank, relativePath)
      <- uses (workingFile . sourceTokens)
              (tail >>> Token.takeParagraph
                    >>> span Token.isBlank
                    >>> second (takeWhile (not . Token.isBlank)))
  let relativePathStr = join $ map Token.unquote relativePath
  directory <- use (workingFile . sourcePath) >>= return . maybe "./" FilePath.dropFileName
  let path = FilePath.normalise $ FilePath.combine directory relativePathStr

  forM_ (instructionToken:blank) $ \_ -> dropToken
  lift (MonadStorage.maybeReadFile path) >>= \case
    Just contents -> do
      forM_ contents $ \ch -> do
        lookupRuleInDynamicScope (^. escapeEnv . at ch) >>= \case
          Just replacement -> do
            appendString replacement
          Nothing ->
            appendChar ch
      forM_ relativePath $ \_ -> dropToken
    Nothing -> do
      workingFile . sourceToken .= join (map Token.toString relativePath)
      noSuchFile path
      forM_ relativePath $ \_ -> dropToken

renderInstruction :: Monad m => RendererT m ()
renderInstruction = do
  instructionToken <- uses (workingFile . sourceTokens) head
  let relativeRuleName = Token.toString instructionToken & tail

  lookupAbsoluteRuleNameInRelativeScope relativeRuleName >>= \case
    Just absoluteRuleName -> do
      dropToken
      openAbsoluteRule absoluteRuleName
      blanks <- uses (workingFile . sourceTokens) (takeWhile Token.isBlank)
      forM_ blanks $ \_ -> dropToken
    Nothing -> do
      ifThenElseM (doesRuleExistInDynamicScope relativeRuleName)
        (do
          closeWorkingRule)
        (do
          workingFile . sourceToken .= Token.toString instructionToken
          failure ("not in scope `" ++ relativeRuleName ++ "'")
          dropToken
          blanks <- uses (workingFile . sourceTokens) (takeWhile Token.isBlank)
          forM_ blanks $ \_ -> dropToken)

renderQuote :: Monad m => RendererT m ()
renderQuote = do
  s <- uses (workingFile . sourceTokens) (head >>> Token.unquote)
  dropToken
  forM_ s $ \ch -> do
    lookupRuleInDynamicScope (^. escapeEnv . at ch) >>= \case
      Just replacement -> do
        appendString replacement
      Nothing ->
        appendChar ch

renderWord :: Monad m => RendererT m ()
renderWord = do
  word <- uses (workingFile . sourceTokens) head
  let asString = Token.toString word

  lookupRuleInDynamicScope (^. macroEnv . at asString) >>= \case
    Just file -> do
      call <- use workingFile
      dropToken
      openFileFrom call file
    Nothing -> do
      workingFile . sourceTokens %= tail

      workingFile . sourceTokens %= (map (\ch -> Token Word [ch]) asString ++)
      foreach asString $ \ch -> do
        lift (lookupRuleInDynamicScope (^. macroEnv . at [ch])) >>= \case
          Just file -> do
            lift $ do
              workingFile . sourceToken .= [ch]
              call <- use workingFile
              dropToken
              openFileFrom call file
            exit
          Nothing -> do
            lift $ do
              dropToken
              lookupRuleInDynamicScope (^. escapeEnv . at ch) >>= \case
                Just replacement -> do
                  appendString replacement
                Nothing -> do
                  appendChar ch

renderParagraphBreak :: Monad m => RendererT m ()
renderParagraphBreak = do
  lookupAbsoluteRuleNameInRelativeScope "paragraph" >>= \case
    Just absoluteRuleName -> do
      dropToken
      openAbsoluteRule absoluteRuleName
      blanks <- uses (workingFile . sourceTokens) (takeWhile Token.isBlank)
      forM_ blanks $ \_ -> dropToken
    Nothing -> do
      ifThenElseM (doesRuleExistInDynamicScope "paragraph")
        (do
          closeWorkingRule)
        (do
          dropToken
          blanks <- uses (workingFile . sourceTokens) (takeWhile Token.isBlank)
          forM_ blanks $ \_ -> dropToken)

renderLineBreak :: Monad m => RendererT m ()
renderLineBreak = do
  renderText

renderSpace :: Monad m => RendererT m ()
renderSpace = do
  renderText

renderBad :: Monad m => RendererT m ()
renderBad = do
  Token (Bad why) s <- uses (workingFile . sourceTokens) head
  workingFile . sourceToken .= s
  failure why
  dropToken

dropToken :: Monad m => RendererT m ()
dropToken = use (workingFile . sourceTokens) >>= \case
  [] ->
    error "Language.Paradocs.dropToken: sourceTokens is empty"
  (Empty:_) -> do
    workingFile . sourceTokens %= tail
    return ()
  ((Token _ s):_) -> do
    workingFile . sourceTokens %= tail
    modify $ RendererState.stepPosition s

renderFromFile :: MonadStorage m => FilePath -> m (Maybe Rendered)
renderFromFile path = do
  MonadStorage.maybeReadFile path >>= \case
    Just contents -> do
      let file = File.empty & sourceCode .~ contents
                            & sourceLine .~ head (lines contents)
                            & sourcePath ?~ path
                            & sourceTokens .~ Token.tokenize contents
      let rendererState = RendererState.empty & workingFile .~ file
      runRendererT render rendererState >>= return . Just
    Nothing -> do
      return Nothing

renderString :: MonadStorage m => String -> m Rendered
renderString contents = do
  let file = File.empty & sourceCode .~ contents
                        & sourceLine .~ head (lines contents)
                        & sourceTokens .~ Token.tokenize contents
  let rendererState = RendererState.empty & workingFile .~ file
  runRendererT render rendererState

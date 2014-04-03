{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE NamedFieldPuns             #-}

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
import qualified Data.Char                      as Char
import qualified Data.ByteString.UTF8           as ByteString
import qualified Text.Trifecta                  as Trifecta
import qualified Text.Trifecta.Delta            as TrifectaDelta
import qualified System.IO                      as System
import qualified System.Exit                    as Exit
import qualified System.Directory               as Directory
import qualified System.FilePath.Posix          as FilePath
import qualified System.Environment             as Environment
import qualified Text.PrettyPrint.ANSI.Leijen   as Leijen

data Token
  = Word            -- /\S+/
    { tokenAsStr    :: String
    , line          :: String
    , pos           :: TrifectaDelta.Delta
    }
  | Quote           -- /\\./, /".*?"/
    { tokenAsStr    :: String
    , line          :: String
    , pos           :: TrifectaDelta.Delta
    }
  | MultiQuote      -- /(""+).*?\1/m
    { tokenAsStr    :: String
    , line          :: String
    , pos           :: TrifectaDelta.Delta
    }
  | Space
    { tokenAsStr    :: String
    , line          :: String
    , pos           :: TrifectaDelta.Delta
    }
  | LineBreak
    { tokenAsStr    :: String
    , line          :: String
    , pos           :: TrifectaDelta.Delta
    }
  | ChangeRule      -- /%\S+/
    { tokenAsStr    :: String
    , line          :: String
    , pos           :: TrifectaDelta.Delta
    }
  | Bad
    { tokenAsStr    :: String
    , line          :: String
    , pos           :: TrifectaDelta.Delta
    , errMsg        :: String
    }
  deriving (Show)

data Instr
  = DefRule                 -- %rule rule-token_1 rule-token_2 ... rule-token_n
  | DefExtend               -- %extend rule-token_1 rule-token_2 ... rule-token_n
  | DefMacro                -- %def identifier-token replacement-token_1 replacement-token_2 ... replacement-token_n
  | DefEscape               -- %escape identifier-token replacement-token_1 replacement-token_2 ... replacement-token_n
  | DefRenderBefore         -- %render-before render-token_1 render-token_2 ... render-token_n
  | DefRenderAfter          -- %render-after render-token_1 render-token_2 ... render-token_n
  | DefIndent               -- %indent indent-token_1 indent-token_2 ... indent-token_n
  | Include                 -- %include file-path-token_1 file-path-token_2 ... file-path-token_n
  deriving (Show)

data Stat
  = Paragraph [Token]
  | Instr Instr [Token]
  deriving (Show)

data AST = Document [Stat]
  deriving (Show)

data RenderedText
  = RenderedLine String
  deriving (Show)

type Rendered = [RenderedText]

type MacroMap = HashMap.HashMap String [Token]

type EscapeMap = HashMap.HashMap Char String

data Rule
  = Rule
    { ruleMacroMap      :: MacroMap
    , ruleAncestors     :: [AbsoluteRuleName]
    , ruleEscapeMap     :: EscapeMap
    , ruleRenderBefore  :: String
    , ruleRenderAfter   :: String
    , ruleIndent        :: String
    }
    deriving (Show)

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

type RelativeRuleName = String
type AbsoluteRuleName = [RelativeRuleName]

type RuleMap = HashMap.HashMap AbsoluteRuleName Rule

data StackValue
  = StackValue
    { stackWorkingRuleName :: AbsoluteRuleName
    , stackRendered :: Rendered
    }

type Stack = [StackValue]

data RendererState
  = RendererState
    { _code :: AST
    , _rendered :: Rendered
    , _stack :: Stack
    , _ruleMap :: RuleMap
    , _workingRuleName :: AbsoluteRuleName
    , _defRuleName :: AbsoluteRuleName
    }
makeLenses ''RendererState

newtype Renderer a
  = Renderer
    {
      unRenderer :: StateT RendererState IO a
    }
  deriving (Functor
            , Applicative
            , Monad
            , MonadState RendererState
            , MonadIO
            )

data ParserState
  = ParserState

newtype Parser a
  = Parser
    {
      unParser :: StateT ParserState Trifecta.Parser a
    }
  deriving (Functor
            , Applicative
            , Alternative
            , Monad
            , MonadPlus
            , MonadState ParserState
            , Trifecta.Parsing
            , Trifecta.DeltaParsing
            , Trifecta.CharParsing
            , Trifecta.TokenParsing
            )

runParser :: Parser a -> ParserState -> Trifecta.Parser a
runParser = evalStateT . unParser

runRenderer :: Renderer a -> RendererState -> IO a
runRenderer = evalStateT . unRenderer

reservedNames :: [String]
reservedNames = [ "rule"
                , "extend"
                , "def"
                , "escape"
                , "render-before"
                , "render-after"
                , "indent"
                , "include"
                ]

parseSingleton :: Parser a -> Parser [a]
parseSingleton p = do { x <- p ; return [x] }

backslashQuote :: Parser Token
backslashQuote = do
  line <- Trifecta.line
  pos <- Trifecta.position
  _ <- Trifecta.char '\\'
  ch <- Trifecta.anyChar
  return $ Quote { tokenAsStr = ['\\', ch]
                  , line = ByteString.toString line
                  , pos = pos
                  }

multiDoubleQuote :: Parser Token
multiDoubleQuote = do
    line <- Trifecta.line
    pos <- Trifecta.position
    beginMarkParts1 <- Trifecta.string "\"\""
    beginMarkParts2 <- many (Trifecta.char '"')
    _ <- optional (Trifecta.char '\n')
    let beginMark = beginMarkParts1 ++ beginMarkParts2
    optional (Trifecta.try (parseContents beginMark)) >>= \case
      Just contents -> return $ MultiQuote { tokenAsStr = beginMark ++ contents ++ beginMark
                                        , line = ByteString.toString line
                                        , pos = pos }
      Nothing -> do
        contents <- Trifecta.many (Trifecta.anyChar)
        return $ Bad
                { tokenAsStr = beginMark ++ contents
                , line = ByteString.toString line
                , pos = pos
                , errMsg = "unexpected EOF while looking for matching `" ++ beginMark ++ "'"
                }
  where
    parseContents :: String -> Parser String
    parseContents beginMark = do
      contents <- many (Trifecta.notChar '"')
      maybeEndMark <- Trifecta.some (Trifecta.char '"')
      if beginMark == maybeEndMark
        then return contents
        else do
          contents2 <- parseContents beginMark
          return $ contents ++ maybeEndMark ++ contents2

singleDoubleQuote :: Parser Token
singleDoubleQuote = do
    line <- Trifecta.line
    pos <- Trifecta.position
    _ <- Trifecta.char '"'
    contents <- join <$> many (Trifecta.string "\\\"" <|> parseSingleton (Trifecta.noneOf "\"\n"))
    optional (Trifecta.char '"') >>= \case
      Just _ -> return $ Quote { tokenAsStr = ('"':contents) ++ "\""
                                , line = ByteString.toString line
                                , pos = pos
                                }
      Nothing -> do
        return $ Bad
                { tokenAsStr = '"':contents
                , line = ByteString.toString line
                , pos = pos
                , errMsg = "missing terminating `\"' character"
                }

changeRule :: Parser Token
changeRule = Trifecta.try $ do
  line <- Trifecta.line
  pos <- Trifecta.position
  _ <- Trifecta.char '%'
  optional wordAsString >>= \case
    Just name -> do
      if
        | name `elem` reservedNames
          -> Trifecta.unexpected ('%' : name)
        | otherwise
          -> return $ ChangeRule { tokenAsStr = '%' : name, line = ByteString.toString line, pos = pos }
    Nothing -> do
      return $ Bad { tokenAsStr = "%"
                    , line = ByteString.toString line
                    , pos = pos
                    , errMsg = "there is not a token after `%'"
                    }

wordAsString :: Parser String
wordAsString = some (Trifecta.satisfy (\case
                                        '%' -> False
                                        '\\' -> False
                                        '"' -> False
                                        x -> not (Char.isSpace x)))

wordAsToken :: Parser Token
wordAsToken = do
  line <- Trifecta.line
  pos <- Trifecta.position
  s <- wordAsString
  return $ Word { tokenAsStr = s, line = ByteString.toString line, pos = pos }

token :: Parser Token
token = backslashQuote <|> multiDoubleQuote <|> singleDoubleQuote <|> changeRule <|> wordAsToken

space :: Parser Token
space = do
  line <- Trifecta.line
  pos <- Trifecta.position
  s <- Trifecta.satisfy (\case
                          '\n' -> False
                          ch -> Char.isSpace ch)
  return $ Space { tokenAsStr = [s], line = ByteString.toString line, pos = pos }

newline :: Parser Token
newline = do
  line <- Trifecta.line
  pos <- Trifecta.position
  s <- Trifecta.char '\n'
  return $ LineBreak { tokenAsStr = [s], line = ByteString.toString line, pos = pos }

spacesInParagraph :: Parser [Token]
spacesInParagraph = do
  sp1 <- many space
  nl <- Maybe.maybeToList <$> optional newline
  sp2 <- many space

  return $ join [sp1, nl, sp2]

paragraph :: Parser Stat
paragraph = do
  tokens_ <- join <$> some ((++) <$> parseSingleton token <*> spacesInParagraph)
  let tokens = reverse (dropWhile isSpace (reverse tokens_))
  return $ Paragraph tokens

instr :: Parser Stat
instr = go where
  go = do
    pos <- Trifecta.position
    line <- Trifecta.line
    _ <- Trifecta.char '%'

    instrName <- optional wordAsString

    case instrName of
      Just name -> do
        _ <- spacesInParagraph
        tokens_ <- join <$> many ((++) <$> parseSingleton token <*> spacesInParagraph)
        let tokens = reverse (dropWhile isSpace (reverse tokens_))

        let instrNameAsToken = Word { tokenAsStr = '%':name, pos = pos, line = ByteString.toString line }
        case name of
          "rule" -> return $ Instr DefRule (instrNameAsToken : map (verifyToken "%rule") tokens)
          "extend" -> return $ Instr DefExtend (instrNameAsToken : map (verifyToken "%extend") tokens)
          "def" -> return $ Instr DefMacro (instrNameAsToken : verifyDefMacroTokens tokens)
          "escape" -> return $ Instr DefEscape (instrNameAsToken : verifyDefEscapeTokens tokens)
          "render-before" -> return $ Instr DefRenderBefore (instrNameAsToken : map (verifyToken "%render-before") tokens)
          "render-after" -> return $ Instr DefRenderAfter (instrNameAsToken : map (verifyToken "%render-after") tokens)
          "indent" -> return $ Instr DefIndent (instrNameAsToken : map (verifyToken "%indent") tokens)
          "include" -> return $ Instr Include (instrNameAsToken : map (verifyToken "%include") tokens)
          _ -> Trifecta.unexpected ('%':name)
      Nothing -> Trifecta.unexpected "%"

  verifyToken :: String -> Token -> Token
  verifyToken _ tk@(Word {}) = tk
  verifyToken _ tk@(Quote {}) = tk
  verifyToken rule (ChangeRule {tokenAsStr, line, pos})
        = Bad { tokenAsStr = tokenAsStr
                , line = line
                , pos = pos
                , errMsg = "`" ++ rule ++ "' can't contain `" ++ tokenAsStr ++ "'"
                }
  verifyToken _ tk = tk

  verifyDefMacroTokens :: [Token] -> [Token]
  verifyDefMacroTokens [] = []
  verifyDefMacroTokens (token1:tokens) = verifyToken "%def" token1 : tokens

  verifyDefEscapeTokens :: [Token] -> [Token]
  verifyDefEscapeTokens [] = []
  verifyDefEscapeTokens (token1:tokens) = verifyDefEscapeToken token1 : map (verifyToken "%escape") tokens

  verifyDefEscapeToken :: Token -> Token
  verifyDefEscapeToken tk@(Word {tokenAsStr, line, pos})
        | length tokenAsStr == 1 = tk
        | otherwise = Bad { tokenAsStr = tokenAsStr
                            , line = line
                            , pos = pos
                            , errMsg = "a character expected but got " ++ show (length tokenAsStr) ++ " characters"
                            }
  verifyDefEscapeToken tk@(Quote {tokenAsStr, line, pos})
        | length (unquote tokenAsStr) == 1 = tk
        | otherwise = Bad { tokenAsStr = tokenAsStr
                            , line = line
                            , pos = pos
                            , errMsg = "a character expected but got " ++ show (length (unquote tokenAsStr)) ++ " characters"
                            }
  verifyDefEscapeToken tk
        = Bad { tokenAsStr = tokenAsStr tk
                , line = line tk
                , pos = pos tk
                , errMsg = "a character expected but got `" ++ tokenAsStr tk ++ "'"
                }

document :: Parser AST
document = Document <$> many (Trifecta.spaces *> (paragraph <|> instr))

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

unquote :: String -> String
unquote ('\\':x:xs) = x : unquote xs
unquote ('"':xs)    = unquote xs
unquote (x:xs)      = x : unquote xs
unquote []          = []

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

isSpace :: Token -> Bool
isSpace (Space {}) = True
isSpace (LineBreak {}) = True
isSpace (Word {}) = False
isSpace (Quote {}) = False
isSpace (MultiQuote {}) = False
isSpace (ChangeRule {}) = False
isSpace (Bad {}) = False

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
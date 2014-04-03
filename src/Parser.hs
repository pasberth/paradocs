{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Parser where

import           Control.Applicative
import           Control.Monad.State
import qualified Data.ByteString.UTF8           as ByteString
import qualified Text.Trifecta                  as Trifecta
import qualified Text.Trifecta.Delta            as TrifectaDelta
import qualified Data.Char                      as Char
import qualified Data.Maybe                     as Maybe
import           Types
import           Util

runParser :: Parser a -> ParserState -> Trifecta.Parser a
runParser = evalStateT . unParser

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


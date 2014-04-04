{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE OverloadedStrings    #-}

module Language.Paradocs.Token where

import           Control.Applicative
import           Control.Monad
import qualified Data.List.Split                as Split
import qualified Data.Char                      as Char
import qualified Data.String                    as String
import qualified Language.Paradocs.RuleName     as RuleName

data TokenKind
  = RawBackslash    -- /%\\./
  | RawQuote1       -- /%".*?"/
  | RawQuoteN       -- /%(""+).*?\1/
  | Instruction     -- /%\S+/
  | Backslash       -- /\\./
  | Quote1          -- /".*?"/
  | QuoteN          -- /(""+).*?\1/
  | Word            -- /[^\\"%\s]+/
  | ParagraphBreak  -- /\n\n/
  | LineBreak       -- /\n/
  | Space           -- /\s/
  | Bad             -- otherwise
        String      -- why error
  deriving (Show)

data Token a
  = Token TokenKind a
  | Empty
  deriving (Functor, Show)

toString :: String.IsString a => Token a -> a
toString (Token _ s)  = s
toString Empty        = ""

unquote :: Token String -> String
unquote (Token Backslash s) = unquoteBackslash s
unquote (Token Quote1 s) = unquoteQuote1 s
unquote (Token QuoteN s) = unquoteQuoteN s
unquote (Token RawBackslash s) = unquoteBackslash $ tail s
unquote (Token RawQuote1 s) = unquoteQuote1 $ tail s
unquote (Token RawQuoteN s) = unquoteQuoteN $ tail s
unquote (Token _ s) = s
unquote Empty = ""

unquoteBackslash :: String -> String
unquoteBackslash ('\\':ch:s) = ch : unquoteBackslash s
unquoteBackslash (ch:s) = ch : unquoteBackslash s
unquoteBackslash [] = []

unquoteQuote1 :: String -> String
unquoteQuote1 = unquoteBackslash . init . tail

unquoteQuoteN :: String -> String
unquoteQuoteN s = do
  let mark = takeWhile (=='"') s
  reverse $ drop (length mark) $ reverse $ drop (length mark) s

isRawBackslash :: Token a -> Bool
isRawBackslash (Token RawBackslash _) = True
isRawBackslash _                      = False

isRawQuote1 :: Token a -> Bool
isRawQuote1 (Token RawQuote1 _) = True
isRawQuote1 _                   = False

isRawQuoteN :: Token a -> Bool
isRawQuoteN (Token RawQuoteN _) = True
isRawQuoteN _                   = False

isInstruction :: Token a -> Bool
isInstruction (Token Instruction _) = True
isInstruction _                     = False

isBackslash :: Token a -> Bool
isBackslash (Token Backslash _) = True
isBackslash _                   = False

isQuote1 :: Token a -> Bool
isQuote1 (Token Quote1 _)   = True
isQuote1 _                  = False

isQuoteN :: Token a -> Bool
isQuoteN (Token QuoteN _)   = True
isQuoteN _                  = False

isWord :: Token a -> Bool
isWord (Token Word _) = True
isWord _              = False

isParagraphBreak :: Token a -> Bool
isParagraphBreak (Token ParagraphBreak _) = True
isParagraphBreak _                        = False

isLineBreak :: Token a -> Bool
isLineBreak (Token LineBreak _) = True
isLineBreak _                   = False

isSpace :: Token a -> Bool
isSpace (Token Space _) = True
isSpace _               = False

isBad :: Token a -> Bool
isBad (Token (Bad _) _) = True
isBad _             = False

isEmpty :: Token a -> Bool
isEmpty Empty = True
isEmpty _     = False

isRaw :: Token a -> Bool
isRaw tk = isRawBackslash tk || isRawQuote1 tk || isRawQuoteN tk

isBlank :: Token a -> Bool
isBlank tk = isSpace tk || isLineBreak tk

isParagraphLevelInstruction :: (Eq a, String.IsString a) => Token a -> Bool
isParagraphLevelInstruction (Token Instruction "%rule")    = True
isParagraphLevelInstruction (Token Instruction "%extend")  = True
isParagraphLevelInstruction (Token Instruction "%def")     = True
isParagraphLevelInstruction (Token Instruction "%escape")  = True
isParagraphLevelInstruction (Token Instruction "%before")  = True
isParagraphLevelInstruction (Token Instruction "%after")   = True
isParagraphLevelInstruction (Token Instruction "%indent")  = True
isParagraphLevelInstruction _                              = False

isValidInRuleName :: Token a -> Bool
isValidInRuleName tk = isWord tk || isBlank tk

isValidAsRuleName :: [Token a] -> Bool
isValidAsRuleName = all isValidInRuleName

toAbsoluteRuleName :: [Token String] -> RuleName.AbsoluteRuleName
toAbsoluteRuleName tks
  | isValidAsRuleName tks = reverse $ map (join . map toString) $ Split.wordsBy isBlank tks
  | otherwise = error "Language.Paradocs.Token.toAbsoluteRuleName: invalid rule name"

token :: String -> Token String
token ('%':s) = case token s of
                    (Token QuoteN q)     -> (Token RawQuoteN ('%':q))
                    (Token Quote1 q)     -> (Token RawQuote1 ('%':q))
                    (Token Backslash q)  -> (Token RawBackslash ('%':q))
                    (Token Word q)       -> (Token Instruction ('%':q))
                    _                    -> Token (Bad "there is not a token after `%'") "%"

token s@('"':'"':_) = (takeWhile (=='"') s ++) <$> uncurry f (span (=='"') s) where

  f :: String -> String -> Token String
  f mark = g mark mark

  g :: String -> String -> String -> Token String
  g begin ('"':_)       []        = Token (Bad ("unexpected EOF while looking for matching `" ++ begin ++ "'")) []
  g begin ('"':mark)    ('"':s')   = ('"':) <$> g begin mark s'
  g begin ('"':_)       (ch:s')    = (ch:) <$> g begin begin s'
  g begin []            ('"':s')   = ('"':) <$> g begin begin s'
  g _     []            []        = Token QuoteN []
  g _     []            (_:_)     = Token QuoteN []
  g _     _             _         = error "Language.Paradocs.Token.token.g: error occurred"

token ('"':s) = ('"':) <$> f s where

  f []        = Token (Bad "missing terminating `\"' character") []
  f ('\\':ch:s') = (\x -> '\\':ch:x) <$> f s'
  f ('\n':_)  = Token (Bad "missing terminating `\"' character") []
  f ('"':_)   = Token Quote1 "\""
  f (ch:s')    = (ch:) <$> f s'

token ('\\':ch:_) = Token Backslash ['\\', ch]
token s@('\n':'\n':_) = Token ParagraphBreak $ takeWhile (=='\n') s
token ('\n':_) = Token LineBreak "\n"
token [] = Empty
token s
  | Char.isSpace (head s) = Token Space $ takeWhile (\ch -> Char.isSpace ch && ch /= '\n') s
  | otherwise = Token Word $ takeWhile (\ch -> not (Char.isSpace ch || ch `elem` "\\\"%" )) s

tokenize :: String -> [Token String]
tokenize code =
  case token code of
    Empty -> do
      []
    tk@(Token _ s) -> do
      let code' = drop (length s) code
      tk : tokenize code'

takeParagraph :: (Eq a, String.IsString a) => [Token a] -> [Token a]
takeParagraph = takeWhile (\tk -> not (isParagraphBreak tk || isParagraphLevelInstruction tk))

dropParagraph :: (Eq a, String.IsString a) => [Token a] -> [Token a]
dropParagraph = dropWhile (\tk -> not (isParagraphBreak tk || isParagraphLevelInstruction tk))

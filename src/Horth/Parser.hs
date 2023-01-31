module Horth.Parser (horthParser, Horth.Parser.parse) where

import Control.Applicative (asum)
import Control.Monad (guard, void)
import Data.ByteString.Char8 qualified as Char8
import Data.Char (isSpace, ord)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Megaparsec (
  ParseErrorBundle,
  Parsec,
  ShowErrorComponent (showErrorComponent),
  SourcePos,
  anySingle,
  between,
  eof,
  getSourcePos,
  many,
  optional,
  parse,
  satisfy,
  some,
  try,
  (<?>),
  (<|>),
 )
import Text.Megaparsec.Char (char, eol, space1)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Megaparsec.Char.Lexer qualified as L

import Horth.Types

data HParseError = HParseError String
  deriving stock (Show, Eq, Ord)

instance ShowErrorComponent HParseError where
  showErrorComponent (HParseError s) = s

type Parser = Parsec HParseError Text

parse :: FilePath -> Text -> Either (ParseErrorBundle Text HParseError) [Ast]
parse = Text.Megaparsec.parse horthParser

horthParser :: Parser [Ast]
horthParser = do
  ast <- many horthP
  eof
  pure ast

horthP :: Parser Ast
horthP =
  asum $
    fmap
      try
      [ procP
      , ifP
      , numLitP
      , boolLitP
      , charP
      , renameP -- Must be before strPtrLitP
      , strPtrLitP
      , keywordP "add" (AstIntr Add)
      , keywordP "sub" (AstIntr Sub)
      , keywordP "mul" (AstIntr Mul)
      , keywordP "div" (AstIntr Div)
      , keywordP "eqi" (AstIntr EqI)
      , keywordP "not" (AstIntr Not)
      , keywordP "dup" (AstIntr Dup)
      , keywordP "swap" (AstIntr Swap)
      , keywordP "pop" (AstIntr Pop)
      , keywordP "drop" (AstIntr Pop)
      , keywordP "over" (AstIntr Over)
      , keywordP "rot" (AstIntr Rot)
      , keywordP "read1" (AstIntr Read1)
      , keywordP "read8" (AstIntr Read8)
      , keywordP "write1" (AstIntr Write1)
      , keywordP "write8" (AstIntr Write8)
      , keywordP "mem" (AstIntr Mem)
      , keywordP "syscall1" (AstIntr SysCall1)
      , keywordP "syscall2" (AstIntr SysCall2)
      , keywordP "syscall3" (AstIntr SysCall3)
      , keywordP "syscall4" (AstIntr SysCall4)
      , keywordP "syscall5" (AstIntr SysCall5)
      , keywordP "syscall6" (AstIntr SysCall6)
      , keywordP "unsafe_mk_ptr" (AstIntr UnsafeMkPtr)
      , includeP
      , holeP
      , nameP
      ]

procP :: Parser Ast
procP = do
  procPos <- getSourcePos
  constr <-
    asum
      [ symbol "proc" $> AstProc False
      , symbol "macro" $> AstProc True
      ]
  procName <- lexeme (Text.pack <$> some notSpaceChar)
  (inTy, outTy) <- parens $ do
    inTy <- many (namedTyParser <|> unnamedTyParser)
    void $ symbol "->"
    outTy <- many (namedTyParser <|> unnamedTyParser)
    pure (inTy, outTy)

  procAst <- many horthP

  void $ symbol "end"
  pure $ constr procName (TypeStack inTy) (TypeStack outTy) procAst procPos

ifP :: Parser Ast
ifP = do
  ifPos <- getSourcePos
  void $ symbol "if"

  ifAst <- many horthP

  else' <- optional $ symbol "else"
  elseAst <- case else' of
    Nothing -> pure []
    Just _ -> many horthP

  void $ symbol "end"
  pure $ AstIf ifAst elseAst ifPos

nameP :: Parser Ast
nameP = lexeme $ do
  namePos <- getSourcePos
  name <- Text.pack <$> some notSpaceChar
  guard $ notElem name ["proc", "macro", "if", "else", "end"]
  pure $ AstName name namePos

tyParser :: Parser HType
tyParser = do
  asum
    [ HInt <$ symbol "int"
    , HBool <$ symbol "bool"
    , HPtr <$ symbol "ptr"
    ]

unnamedTyParser :: Parser (HType, Maybe Text)
unnamedTyParser = (,Nothing) <$> tyParser

namedTyParser :: Parser (HType, Maybe Text)
namedTyParser = lexeme $ parens $ do
  tyName <- Text.pack <$> some notSpaceChar
  space
  void $ symbol ":="
  ty <- tyParser
  pure (ty, Just tyName)

notSpaceChar :: Parser Char
notSpaceChar = satisfy (not . isSpace) <?> "not whitespace"
{-# INLINE notSpaceChar #-}

numLitP :: Parser Ast
numLitP = lexeme $ do
  numPos <- getSourcePos
  num <- signed (pure ()) decimal
  return $ AstPushLit (LitInt num) numPos

boolLitP :: Parser Ast
boolLitP = do
  asum
    [ keywordP "true" $ AstPushLit (LitBool True)
    , keywordP "false" $ AstPushLit (LitBool False)
    ]

strPtrLitP :: Parser Ast
strPtrLitP = lexeme $ do
  strPos <- getSourcePos
  void $ char '"'
  str <- many (satisfy (/= '"') <?> "not double quote")
  void $ char '"'
  return $ AstPushLit (LitStr $ Char8.pack str) strPos

charP :: Parser Ast
charP = lexeme $ do
  charPos <- getSourcePos
  void $ char '\''
  c <- anySingle
  void $ char '\''
  return $ AstPushLit (LitInt $ fromIntegral $ ord c) charPos

keywordP :: Show ast => Text -> (SourcePos -> ast) -> Parser ast
keywordP keyword ast = lexeme $ do
  pos <- getSourcePos
  void $ L.symbol (space1 <|> void eol <|> eof) keyword
  pure $ ast pos

renameP :: Parser Ast
renameP = lexeme $ do
  pos <- getSourcePos
  void $ char '"'
  name <- some (satisfy (/= '"') <?> "not double quote")
  void $ char '"'
  space
  void $ L.symbol (space1 <|> void eol <|> eof) "rename"
  pure $ AstIntr (Rename $ Text.pack name) pos

includeP :: Parser Ast
includeP = lexeme $ do
  pos <- getSourcePos
  void $ symbol "#include"
  path <- some notSpaceChar
  pure $ AstInclude path pos

holeP :: Parser Ast
holeP = lexeme $ do
  pos <- getSourcePos
  void $ char '_'
  holeName <- ("_" <>) . Text.pack <$> many notSpaceChar
  pure $ AstHole holeName pos

-- Helpers

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

symbol :: Text -> Parser Text
symbol = L.symbol space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

space :: Parser ()
space = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

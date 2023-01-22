module Horth.Parser (horthParser, Horth.Parser.parse) where

import Control.Applicative (asum)
import Control.Monad (guard, void)
import Data.ByteString.Char8 qualified as Char8
import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Megaparsec
import Text.Megaparsec.Char

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
      , keywordP "over" (AstIntr Over)
      , keywordP "printi" (AstIntr PrintI)
      , keywordP "printb" (AstIntr PrintB)
      , keywordP "read1" (AstIntr Read1)
      , keywordP "write1" (AstIntr Write1)
      , keywordP "mem" (AstIntr Mem)
      , keywordP "syscall3" (AstIntr SysCall3)
      , holeP
      , includeP
      , nameP
      ]

procP :: Parser Ast
procP = do
  procPos <- getSourcePos
  void $ string "proc"
  whiteSpaceP
  procName <- Text.pack <$> some notSpaceChar
  whiteSpaceP
  void $ char '('
  void $ optional whiteSpaceP
  inTy <- many tyParser
  void $ string "->"
  void $ optional whiteSpaceP
  outTy <- many tyParser
  void $ optional whiteSpaceP
  void $ char ')'
  whiteSpaceP

  procAst <- many horthP

  endPos <- getSourcePos
  void $ string "end"
  whiteSpaceEndP
  pure $ AstProc procName inTy outTy procAst procPos endPos

ifP :: Parser Ast
ifP = do
  ifPos <- getSourcePos
  void $ string "if" >> whiteSpaceP

  ifAst <- many horthP

  endPos <- getSourcePos
  void $ string "end" >> whiteSpaceEndP
  pure $ AstIf ifAst ifPos endPos

nameP :: Parser Ast
nameP = do
  namePos <- getSourcePos
  name <- Text.pack <$> some notSpaceChar
  guard $ notElem name ["proc", "if", "end"]
  whiteSpaceEndP
  pure $ AstName name namePos

tyParser :: Parser HType
tyParser = do
  ty <-
    asum
      [ HInt <$ string "int"
      , HBool <$ string "bool"
      , HPtr <$ string "ptr"
      ]
  void $ optional whiteSpaceP
  pure ty

notSpaceChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
notSpaceChar = satisfy (not . isSpace) <?> "not white space"
{-# INLINE notSpaceChar #-}

whiteSpaceP :: Parser ()
whiteSpaceP = skipSome spaceChar

whiteSpaceEndP :: Parser ()
whiteSpaceEndP = whiteSpaceP <|> eof

numLitP :: Parser Ast
numLitP = do
  numPos <- getSourcePos
  num <- some digitChar
  whiteSpaceEndP
  return $ AstPushLit (LitInt (read num)) numPos

boolLitP :: Parser Ast
boolLitP = do
  asum
    [ keywordP "true" $ AstPushLit (LitBool True)
    , keywordP "false" $ AstPushLit (LitBool False)
    ]

strPtrLitP :: Parser Ast
strPtrLitP = do
  strPos <- getSourcePos
  void $ char '"'
  str <- many (satisfy (/= '"') <?> "not double quote")
  void $ char '"'
  whiteSpaceEndP
  return $ AstPushLit (LitStr $ Char8.pack str) strPos

keywordP :: Show ast => Text -> (SourcePos -> ast) -> Parser ast
keywordP keyword ast = do
  pos <- getSourcePos
  void $ string keyword
  whiteSpaceEndP
  pure $ ast pos

includeP :: Parser Ast
includeP = do
  pos <- getSourcePos
  void $ string "#include"
  whiteSpaceP
  path <- some notSpaceChar
  whiteSpaceEndP
  pure $ AstInclude path pos

holeP :: Parser Ast
holeP = do
  pos <- getSourcePos
  void $ string "_"
  holeName <- ("_" <>) . Text.pack <$> many notSpaceChar
  whiteSpaceEndP
  pure $ AstHole holeName pos

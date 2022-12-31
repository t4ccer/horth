module Parser (horthParser) where

import Control.Applicative (asum)
import Control.Monad (guard, void)
import Data.Char (isSpace)
import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Megaparsec
import Text.Megaparsec.Char

import Types

data HParseError = HParseError String
  deriving stock (Show, Eq, Ord)

instance ShowErrorComponent HParseError where
  showErrorComponent (HParseError s) = s

type Parser = Parsec HParseError Text

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
      , nameP
      ]

procP :: Parser Ast
procP = do
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

  void $ string "end"
  whiteSpaceEndP
  pure $ AstProc procName inTy outTy procAst

ifP :: Parser Ast
ifP = do
  void $ string "if" >> whiteSpaceP

  ifAst <- many horthP

  void $ string "end" >> whiteSpaceEndP
  pure $ AstIf ifAst

nameP :: Parser Ast
nameP = do
  name <- Text.pack <$> some notSpaceChar
  guard $ notElem name ["proc", "if", "end"]
  whiteSpaceEndP
  pure $ AstName name

tyParser :: Parser HType
tyParser = do
  ty <-
    asum
      [ HInt <$ string "int"
      , HBool <$ string "bool"
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
  num <- some digitChar
  whiteSpaceEndP
  return $ AstPushLit $ LitInt $ read num

boolLitP :: Parser Ast
boolLitP = do
  bool <-
    asum
      [ keywordP "true" $ AstPushLit $ LitBool True
      , keywordP "false" $ AstPushLit $ LitBool False
      ]
  pure bool

keywordP :: Show ast => Text -> ast -> Parser ast
keywordP keyword ast = do
  void $ string keyword
  whiteSpaceEndP
  pure ast

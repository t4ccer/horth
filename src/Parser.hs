module Parser () where

-- import Text.Megaparsec
-- import Text.Megaparsec.Char
-- import Data.Text (Text)
-- import Data.Text qualified as Text
-- import Control.Applicative (asum)
-- import Control.Monad (void)
-- import Data.Functor.Identity (Identity (Identity))

-- import Types

-- data HParseError = HParseError String
--   deriving stock (Show, Eq, Ord)

-- instance ShowErrorComponent HParseError where
--   showErrorComponent (HParseError s) = s

-- type Parser = Parsec HParseError Text

-- horthParser :: Parser [OpCodeWithLabels]
-- horthParser = do
--   opCodes <- many opCode
--   eof
--   pure opCodes

-- opCode :: Parser OpCodeWithLabels
-- opCode = asum $ fmap try $
--   [ labelDeclP
--   , labelRefP
--   , numLitP
--   , boolLitP

--   , keywordP "add" $ Intr Add
--   , keywordP "sub" $ Intr Sub
--   , keywordP "mul" $ Intr Mul
--   , keywordP "div" $ Intr Div
--   , keywordP "eqi" $ Intr EqI
--   , keywordP "not" $ Intr Not
--   , keywordP "jet" $ Intr Jet
--   , keywordP "dup" $ Intr Dup
--   , keywordP "swap" $ Intr Swap
--   , keywordP "pop" $ Intr Pop
--   , keywordP "over" $ Intr Over

--   , nameP
--   ]

-- whiteSpace :: Parser ()
-- whiteSpace = skipSome spaceChar <|> eof

-- labelDeclP :: Parser OpCodeWithLabels
-- labelDeclP = do
--   label <- some alphaNumChar
--   void $ char ':'
--   whiteSpace
--   pure $ LabelDecl $ Identity $ Text.pack label

-- labelRefP :: Parser OpCodeWithLabels
-- labelRefP = do
--   void $ char '!'
--   label <- some alphaNumChar
--   whiteSpace
--   pure $ LabelRef $ Identity $ Text.pack label

-- numLitP :: Parser OpCodeWithLabels
-- numLitP = do
--   num <- some digitChar
--   whiteSpace
--   return $ PushLit $ LitInt $ read num

-- boolLitP :: Parser OpCodeWithLabels
-- boolLitP = do
--   bool <- asum
--     [ keywordP "true" $ PushLit $ LitBool True
--     , keywordP "false" $ PushLit $ LitBool False
--     ]
--   pure bool

-- keywordP :: Text -> OpCodeWithLabels -> Parser OpCodeWithLabels
-- keywordP keyword opCode = do
--   void $ string keyword
--   whiteSpace
--   pure opCode

-- nameP :: Parser OpCodeWithLabels
-- nameP = do
--   name <- some alphaNumChar
--   whiteSpace
--   pure $ Name $ Text.pack name

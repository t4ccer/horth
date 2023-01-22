module Horth.Cli (
  Mode (..),
  CompileOpts (..),
  PrettyOpts (..),
  ExeFormat (..),
  getMode,
  prettyFormat,
) where

import Data.List (intercalate)
import Data.Tuple (swap)
import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  command,
  customExecParser,
  eitherReader,
  fullDesc,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  prefs,
  progDesc,
  short,
  showHelpOnError,
  strOption,
  subparser,
  value,
 )

data Mode
  = ModeCompile CompileOpts
  | ModePretty PrettyOpts
  deriving stock (Show, Eq)

data CompileOpts = CompileOpts
  { compileOptsInput :: FilePath
  , compileOptsOutput :: FilePath
  , compileOptsFormat :: ExeFormat
  , compileOptsNasm :: FilePath
  , compileOptsLd :: FilePath
  }
  deriving stock (Show, Eq)

data ExeFormat = ExeFormatElf64
  deriving stock (Show, Eq)

data PrettyOpts = PrettyOpts
  { prettyOptsInput :: FilePath
  }
  deriving stock (Show, Eq)

getMode :: IO Mode
getMode = customExecParser (prefs showHelpOnError) $ info (helper <*> modeP) fullDesc

modeP :: Parser Mode
modeP = subparser (compileP <> prettyP)

compileP :: Mod CommandFields Mode
compileP = command "compile" (ModeCompile <$> info compileOptsP (progDesc "Compile a horth program"))

compileOptsP :: Parser CompileOpts
compileOptsP =
  CompileOpts
    <$> strOption (long "input" <> short 'i' <> metavar "FILE" <> help "Input file")
    <*> strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Output file")
    <*> option (eitherReader parseExeFormat) (long "format" <> short 'f' <> metavar "FORMAT" <> help "Output format")
    <*> strOption (long "nasm" <> metavar "PATH" <> value "nasm" <> help "Path to nasm executable")
    <*> strOption (long "ld" <> metavar "PATH" <> value "ld" <> help "Path to ld executable")

parseExeFormat :: String -> Either String ExeFormat
parseExeFormat format =
  case lookup format formats of
    Just f -> Right f
    Nothing ->
      Left $
        mconcat
          [ "Unknown format: "
          , format
          , ". Supported formats: "
          , intercalate ", " (map fst formats)
          ]

prettyFormat :: ExeFormat -> String
prettyFormat format =
  case lookup format $ fmap swap formats of
    Just f -> f
    Nothing -> error "Unknown format"

formats :: [(String, ExeFormat)]
formats = [("elf64", ExeFormatElf64)]

prettyP :: Mod CommandFields Mode
prettyP = command "pretty" (ModePretty <$> info prettyOptsP (progDesc "Pretty print a horth program"))

prettyOptsP :: Parser PrettyOpts
prettyOptsP =
  PrettyOpts
    <$> strOption (long "input" <> short 'i' <> metavar "FILE" <> help "Input file")

module Horth.Cli (
  Mode (..),
  CompileOpts (..),
  RunOpts (..),
  ExeFormat (..),
  getMode,
) where

import Data.List (intercalate)
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
 )

data Mode
  = ModeCompile CompileOpts
  | ModeRun RunOpts
  deriving stock (Show, Eq)

data CompileOpts = CompileOpts
  { compileOptsInput :: FilePath
  , compileOptsOutput :: FilePath
  , compileOptsFormat :: ExeFormat
  }
  deriving stock (Show, Eq)

data ExeFormat = ExeFormatElf64
  deriving stock (Show, Eq)

data RunOpts = RunOpts
  { runOptsInput :: FilePath
  }
  deriving stock (Show, Eq)

getMode :: IO Mode
getMode = customExecParser (prefs showHelpOnError) $ info (helper <*> modeP) fullDesc

modeP :: Parser Mode
modeP = subparser (compileP <> runP)

compileP :: Mod CommandFields Mode
compileP = command "compile" (ModeCompile <$> info compileOptsP (progDesc "Compile a horth program"))

compileOptsP :: Parser CompileOpts
compileOptsP =
  CompileOpts
    <$> strOption (long "input" <> short 'i' <> metavar "FILE" <> help "Input file")
    <*> strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Output file")
    <*> option (eitherReader parseExeFormat) (long "format" <> short 'f' <> metavar "FORMAT" <> help "Output format")

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
  where
    formats = [("elf64", ExeFormatElf64)]

runP :: Mod CommandFields Mode
runP = command "run" (ModeRun <$> info runOptsP (progDesc "Interpret a horth program"))

runOptsP :: Parser RunOpts
runOptsP =
  RunOpts
    <$> strOption (long "input" <> short 'i' <> metavar "FILE" <> help "Input file")

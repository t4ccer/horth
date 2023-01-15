module Main (main) where

import Control.Monad (void)
import Data.Text.IO qualified as Text
import System.Exit (ExitCode (ExitSuccess), exitFailure, exitWith)
import System.FilePath (addExtension)
import System.IO (stderr)
import System.Process.Typed (proc, runProcess)
import Text.Megaparsec.Error (errorBundlePretty)

import Horth.Cli (
  CompileOpts (
    compileOptsFormat,
    compileOptsInput,
    compileOptsOutput
  ),
  ExeFormat (ExeFormatElf64),
  Mode (ModeCompile, ModePretty, ModeRun),
  PrettyOpts (prettyOptsInput),
  RunOpts (runOptsInput),
  getMode,
  prettyFormat,
 )
import Horth.Compiler (compile)
import Horth.Interpreter (interpret)
import Horth.Native.Elf64 (compileElf64)
import Horth.Parser (parse)
import Horth.Pretty (prettyAst)
import Horth.TypeChecker (TypeCheckedAst, typeCheck)

getAst :: FilePath -> IO TypeCheckedAst
getAst fp = do
  sourceCode <- Text.readFile fp
  parsedAst <-
    case parse fp sourceCode of
      Left e -> error $ errorBundlePretty e
      Right ast -> pure ast
  (ast, _) <-
    case typeCheck parsedAst of
      Left err -> do
        Text.hPutStrLn stderr err
        exitFailure
      Right res -> pure res
  pure ast

forwardExitCode :: ExitCode -> IO ()
forwardExitCode = \case
  ExitSuccess -> pure ()
  code -> exitWith code

main :: IO ()
main = do
  mode <- getMode
  case mode of
    ModeCompile opts -> do
      ast <- getAst opts.compileOptsInput
      let compileNative =
            case opts.compileOptsFormat of
              ExeFormatElf64 -> compileElf64
      Text.writeFile (addExtension opts.compileOptsOutput ".asm") $ compileNative $ compile ast
      runProcess (proc "nasm" ["-f", prettyFormat opts.compileOptsFormat, addExtension opts.compileOptsOutput ".asm"])
        >>= forwardExitCode
      runProcess (proc "ld" [addExtension opts.compileOptsOutput ".o", "-o", opts.compileOptsOutput])
        >>= forwardExitCode
    ModeRun opts -> do
      ast <- getAst opts.runOptsInput
      void $ interpret $ compile ast
    ModePretty opts -> do
      sourceCode <- Text.readFile opts.prettyOptsInput
      case parse opts.prettyOptsInput sourceCode of
        Left e -> error $ show e
        Right ast -> Text.putStr $ prettyAst ast

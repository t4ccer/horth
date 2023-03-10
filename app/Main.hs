module Main (main) where

import Data.Text.IO qualified as Text
import System.Directory (canonicalizePath)
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
  Mode (ModeCompile),
  getMode,
  prettyFormat,
 )
import Horth.Compiler (compile)
import Horth.Includes (resolveIncludes)
import Horth.Native.Elf64 (compileElf64)
import Horth.Parser (parse)
import Horth.TypeChecker (TypeCheckedAst, typeCheck)

getAst :: FilePath -> IO TypeCheckedAst
getAst fp' = do
  fp <- canonicalizePath fp'
  sourceCode <- Text.readFile fp
  parsedAst <-
    case parse fp sourceCode of
      Left e -> error $ errorBundlePretty e
      Right ast -> resolveIncludes ast
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

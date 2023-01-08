module Main (main) where

import Control.Monad (void)
import Data.Text.IO qualified as Text
import System.Exit (exitFailure)
import System.IO (stderr)

import Horth.Cli (
  CompileOpts (
    compileOptsFormat,
    compileOptsInput,
    compileOptsOutput
  ),
  ExeFormat (ExeFormatElf64),
  Mode (ModeCompile, ModeRun),
  RunOpts (runOptsInput),
  getMode,
 )
import Horth.Compiler (compile)
import Horth.Interpreter (interpret)
import Horth.Native.Elf64 (compileElf64)
import Horth.Parser (parse)
import Horth.TypeChecker (TypeCheckedAst, typeCheck)

getAst :: FilePath -> IO TypeCheckedAst
getAst fp = do
  sourceCode <- Text.readFile fp
  parsedAst <-
    case parse fp sourceCode of
      Left e -> error $ show e
      Right ast -> pure ast
  (ast, _) <-
    case typeCheck parsedAst of
      Left err -> do
        Text.hPutStrLn stderr err
        exitFailure
      Right res -> pure res
  pure ast

main :: IO ()
main = do
  mode <- getMode
  case mode of
    ModeCompile opts -> do
      ast <- getAst opts.compileOptsInput
      let compileNative =
            case opts.compileOptsFormat of
              ExeFormatElf64 -> compileElf64
      Text.writeFile opts.compileOptsOutput $ compileNative $ compile ast
    ModeRun opts -> do
      ast <- getAst opts.runOptsInput
      void $ interpret $ compile ast

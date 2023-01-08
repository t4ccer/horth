module Main (main) where

import Data.Text.IO qualified as Text
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)

import Horth.Compiler (compile)
import Horth.Interpreter (interpret)
import Horth.Native.X86_64 (compileX86_64)
import Horth.Parser (parse)
import Horth.TypeChecker (typeCheck)

main :: IO ()
main = do
  [inFp, outFp] <- getArgs

  sourceCode <- Text.readFile inFp
  parsedAst <- case parse inFp sourceCode of
    Left e -> error $ show e
    Right ast -> pure ast

  (ast, _) <- case typeCheck parsedAst of
    Left err -> do
      Text.hPutStrLn stderr err
      exitFailure
    Right res -> pure res

  let opCode = compile ast
  _ <- interpret opCode
  Text.writeFile outFp (compileX86_64 opCode)

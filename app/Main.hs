module Main (main) where

import Data.Text.IO qualified as Text
import System.Exit (exitFailure)
import System.IO (stderr)

import Horth.Compiler (compile)
import Horth.Interpreter (interpret)
import Horth.Native.X86_64 (compileX86_64)
import Horth.Parser (parse)
import Horth.TypeChecker (typeCheck)

main :: IO ()
main = do
  putStrLn ""
  let fp = "examples/fac.horth"
  -- let fp = "examples/simple.horth"
  sourceCode <- Text.readFile fp
  parsedAst <- case parse fp sourceCode of
    Left e -> error $ show e
    Right ast -> pure ast

  (ast, ty) <- case typeCheck parsedAst of
    Left err -> do
      Text.hPutStrLn stderr err
      exitFailure
    Right res -> pure res

  putStrLn $ "Program type: " <> show ty

  let opCode = compile ast
  interpret opCode >>= print

  putStrLn ""
  Text.writeFile "/home/t4ccer/repos/github/t4ccer/messing-with-nasm/hello.asm" (compileX86_64 opCode)

module Main (main) where

import Data.Text.IO qualified as Text
import System.IO (stderr)
import System.Exit (exitFailure)

import Horth.Compiler (compile)
import Horth.Parser (parse)
import Horth.TypeChecker (typeCheck)
import Horth.Interpreter (interpret)

main :: IO ()
main = do
  putStrLn ""
  let fp = "examples/fac.horth"
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
  print $ interpret opCode

module Horth.Includes (resolveIncludes) where

import Data.Text.IO qualified as Text
import Horth.Parser (parse)
import System.FilePath (takeDirectory, (</>))
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec.Pos (sourceName)

import Horth.Types

resolveIncludes :: [Ast] -> IO [Ast]
resolveIncludes = fmap mconcat . traverse resolveInclude

resolveInclude :: Ast -> IO [Ast]
resolveInclude (AstInclude fname pos) = do
  let fp = (takeDirectory $ sourceName pos) </> (fname <> ".horth")
  sourceCode <- Text.readFile fp
  case parse fp sourceCode of
    Left e -> error $ errorBundlePretty e
    Right ast -> resolveIncludes ast
resolveInclude ast = pure [ast]

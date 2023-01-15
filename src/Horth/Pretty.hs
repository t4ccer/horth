{-# LANGUAGE MultiWayIf #-}

module Horth.Pretty (prettyAst) where

import Control.Monad.State (State, execState, gets, modify)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder qualified as TextBuilder
import Text.Megaparsec.Pos (Pos, SourcePos (SourcePos, sourceColumn, sourceLine), mkPos, pos1, sourcePosPretty, unPos)

import Horth.Types

data PrettyAstState = PrettyAstState
  { prettyAstStateCurrPos :: SourcePos
  , prettyAstStateAcc :: TextBuilder.Builder
  , prettyAstStateAst :: [Ast]
  }

astPos :: Ast -> SourcePos
astPos (AstPushLit _ pos) = pos
astPos (AstIntr _ pos) = pos
astPos (AstName _ pos) = pos
astPos (AstIf _ pos _) = pos
astPos (AstProc _ _ _ _ pos _) = pos
astPos (AstHole _ pos) = pos

addToPos :: Int -> Pos -> Pos
addToPos n pos = mkPos $ (+ n) $ unPos pos

prettyType :: HType -> Text
prettyType HInt = "int"
prettyType HBool = "bool"
prettyType HPtr = "ptr"
prettyType HAddr = error "prettyType: HAddr: Not implemented"
prettyType (HTypeVar _ _) = error "prettyType: HTypeVar: Not implemented"

prettyTypes :: [HType] -> Text
prettyTypes = Text.intercalate " " . fmap prettyType

prettyAst :: [Ast] -> Text
prettyAst ast =
  toStrict
    . TextBuilder.toLazyText
    . prettyAstStateAcc
    . flip execState (PrettyAstState (SourcePos "" pos1 pos1) mempty ast)
    $ prettyAst'
  where
    newLine :: State PrettyAstState ()
    newLine = do
      modify $ \s ->
        s
          { prettyAstStateCurrPos =
              (prettyAstStateCurrPos s)
                { sourceLine = addToPos 1 $ sourceLine $ prettyAstStateCurrPos s
                , sourceColumn = pos1
                }
          , prettyAstStateAcc = prettyAstStateAcc s <> "\n"
          }

    emit :: Text -> State PrettyAstState ()
    emit t = do
      modify $ \s ->
        s
          { prettyAstStateCurrPos =
              (prettyAstStateCurrPos s)
                { sourceColumn = addToPos (Text.length t) $ sourceColumn $ prettyAstStateCurrPos s
                }
          , prettyAstStateAcc = prettyAstStateAcc s <> TextBuilder.fromText t
          }

    goForward :: SourcePos -> State PrettyAstState ()
    goForward targetPos = do
      currPos <- gets prettyAstStateCurrPos
      if
          | sourceLine currPos /= sourceLine targetPos -> do
              newLine
              goForward targetPos
          | sourceColumn currPos < sourceColumn targetPos -> do
              emit " "
              goForward targetPos
          | (sourceColumn currPos > sourceColumn targetPos)
              || (sourceLine currPos > sourceLine targetPos) -> do
              error $ sourcePosPretty currPos <> ": ERROR: Print invariant violated"
          | otherwise -> pure ()

    prettyAst' :: State PrettyAstState ()
    prettyAst' = do
      ast <- gets prettyAstStateAst
      case ast of
        [] -> pure ()
        (currAst : restAst) -> do
          goForward $ astPos currAst
          case currAst of
            AstPushLit lit _ -> emit $ prettyLit lit
            AstIntr intr _ -> emit $ prettyIntrinsic intr
            AstName name _ -> emit $ name
            AstHole holeName _ -> emit $ holeName
            AstIf ifAst _ endPos -> do
              emit "if"
              modify (\s -> s {prettyAstStateAst = ifAst})
              prettyAst'
              goForward endPos
              emit "end"
              modify (\s -> s {prettyAstStateAst = restAst})
            AstProc procName inTy outTy procAst _procPos endPos -> do
              emit "proc "
              emit procName
              emit " ("
              emit $ prettyTypes inTy
              emit " -> "
              emit $ prettyTypes outTy
              emit ")"
              modify (\s -> s {prettyAstStateAst = procAst})
              prettyAst'
              goForward endPos
              emit "end"
              modify (\s -> s {prettyAstStateAst = restAst})

          modify (\s -> s {prettyAstStateAst = restAst})
          prettyAst'

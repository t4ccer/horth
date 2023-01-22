{-# LANGUAGE RecursiveDo #-}

module Horth.Compiler (CompileError (..), Code (getCode), compile) where

import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadReader, Reader, asks, local, runReader)
import Control.Monad.State (MonadState, StateT, execStateT, gets, modify)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V

import Horth.TypeChecker (TypeCheckedAst (getTypeCheckedAst))
import Horth.Types

data CompileError
  = CompileError Text
  deriving stock (Show, Eq)

data CompilationState = CompilationState
  { compilationStateEmited :: [OpCode]
  , compilationStateNextAddr :: Addr
  , compilationStateLabels :: [(Text, Addr)]
  , compilationStateMacros :: [(Text, [Ast])]
  }
  deriving stock (Show, Eq)

data CompilationEnv = CompilationEnv
  { compilationEnvAst :: NonEmpty Ast
  }
  deriving stock (Show, Eq)

newtype CompilationM a = CompilationM
  { runCompilationM :: (StateT CompilationState (Reader CompilationEnv)) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadReader CompilationEnv, MonadState CompilationState, MonadFix)

newtype Code = Code {getCode :: Vector OpCode}
  deriving stock (Show, Eq)

compile :: TypeCheckedAst -> Code
compile (getTypeCheckedAst -> []) = Code V.empty
compile (getTypeCheckedAst -> (allAst : allAsts)) =
  Code
    . V.fromList
    . reverse
    . compilationStateEmited
    . flip runReader (CompilationEnv (allAst :| allAsts))
    . flip execStateT (CompilationState [] 0 [] [])
    . runCompilationM
    $ compile'
  where
    emit :: OpCode -> CompilationM ()
    emit op =
      modify
        ( \s ->
            s
              { compilationStateEmited = op : compilationStateEmited s
              , compilationStateNextAddr = compilationStateNextAddr s + 1
              }
        )

    -- Continue compiling rest of ast in a block
    continueLinear :: [Ast] -> CompilationM ()
    continueLinear [] = pure ()
    continueLinear (a : as) = local (\e -> e {compilationEnvAst = a :| as}) compile'

    getProcAddr :: Text -> CompilationM (Maybe Addr)
    getProcAddr name = do
      labels <- gets compilationStateLabels
      pure $ lookup name labels

    saveProcAddr :: Text -> Addr -> CompilationM ()
    saveProcAddr name addr =
      modify
        ( \s ->
            s
              { compilationStateLabels = (name, addr) : compilationStateLabels s
              }
        )

    registerMacro :: Text -> [Ast] -> CompilationM ()
    registerMacro name asts =
      modify
        ( \s ->
            s
              { compilationStateMacros = (name, asts) : compilationStateMacros s
              }
        )

    getMacro :: Text -> CompilationM (Maybe [Ast])
    getMacro name = do
      macros <- gets compilationStateMacros
      pure $ lookup name macros

    -- FIXME: proc must be defined before call
    compile' :: CompilationM ()
    compile' = do
      currAst :| restAst <- asks compilationEnvAst
      case currAst of
        AstPushLit lit _ -> do
          emit $ OpCodePushLit lit
          continueLinear restAst
        AstIntr intr _ -> do
          emit $ OpCodeIntr intr
          continueLinear restAst
        AstIf ifAst elseAst _ _ -> mdo
          emit $ OpCodeIntr Not
          emit $ OpCodeIntr (Jet addrAfterIfBranch)

          case nonEmpty ifAst of
            Nothing -> pure ()
            Just ifAst' -> do
              labels <- gets compilationStateLabels
              local (\e -> e {compilationEnvAst = ifAst'}) compile'
              modify (\s -> s {compilationStateLabels = labels})
              emit $ OpCodeIntr (Jmp addrAfterElseBranch)

          addrAfterIfBranch <- gets compilationStateNextAddr

          case nonEmpty elseAst of
            Nothing -> pure ()
            Just elseAst' -> do
              labels <- gets compilationStateLabels
              local (\e -> e {compilationEnvAst = elseAst'}) compile'
              modify (\s -> s {compilationStateLabels = labels})

          addrAfterElseBranch <- gets compilationStateNextAddr

          continueLinear restAst
        AstName name _ -> do
          procAddr <- getProcAddr name
          macroAst <- getMacro name
          case (procAddr, macroAst) of
            (Just addr, _) -> mdo
              emit $ OpCodePushToCallStack callStackRetAddr addr
              callStackRetAddr <- gets compilationStateNextAddr
              continueLinear restAst
            (_, Just macroAst) -> do
              continueLinear (macroAst <> restAst)
            (Nothing, Nothing) -> do
              error "proc/macro not defined"
        AstProc False name _inType _outType procAst _ -> mdo
          emit $ OpCodeIntr (Jmp addrAfterProc)
          procStartAddr <- gets compilationStateNextAddr
          saveProcAddr name procStartAddr
          case nonEmpty procAst of
            Nothing -> pure ()
            Just procAst' -> do
              labels <- gets compilationStateLabels
              local (\e -> e {compilationEnvAst = procAst'}) compile'
              modify (\s -> s {compilationStateLabels = labels})
          emit OpCodePopJmpFromCallStack
          addrAfterProc <- gets compilationStateNextAddr
          continueLinear restAst
        AstProc True macroName _inType _outType macroAst _pos -> do
          registerMacro macroName macroAst
          continueLinear restAst
        AstHole _ _ -> error "Hole in ast. This shouldn't happen after type checking"
        AstInclude _ _ -> error "'include's should be already resolved"

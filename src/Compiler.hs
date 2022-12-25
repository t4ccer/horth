{-# LANGUAGE RecursiveDo #-}

module Compiler (CompileError (..), compileHorth) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadReader, ReaderT, asks, local, runReaderT)
import Control.Monad.State (MonadState, StateT, execStateT, gets, modify)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as V

import Types

data CompileError
  = CompileError Text
  deriving stock (Show, Eq)

data CompilationState = CompilationState
  { compilationStateEmited :: [OpCode]
  , compilationStateNextAddr :: Addr
  , compilationStateLabels :: [(Text, Addr)]
  }
  deriving stock (Show, Eq)

data CompilationEnv = CompilationEnv
  { compilationEnvAst :: NonEmpty Ast
  }
  deriving stock (Show, Eq)

newtype CompilationM a = CompilationM
  { runCompilationM :: (StateT CompilationState (ReaderT CompilationEnv (Either CompileError))) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadReader CompilationEnv, MonadState CompilationState, MonadError CompileError, MonadFix)

-- TODO: No type checking for now
compileHorth :: [Ast] -> Either CompileError Code
compileHorth [] = pure $ Code V.empty
compileHorth (allAst : allAsts) =
  fmap (Code . V.fromList . reverse . compilationStateEmited)
    . flip runReaderT (CompilationEnv (allAst :| allAsts))
    . flip execStateT (CompilationState [] 0 [])
    . runCompilationM
    $ compileHorth'
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
    continueLinear (a : as) = local (\e -> e {compilationEnvAst = a :| as}) compileHorth'

    -- TODO: Location
    getProcAddr :: Text -> CompilationM Addr
    getProcAddr name = do
      labels <- gets compilationStateLabels
      case lookup name labels of
        Nothing ->
          throwError $
            CompileError $
              Text.pack $
                mconcat
                  [ "Unknown procedure: " -- NOTE: Shouldn't happen after type checking
                  , show name
                  ]
        Just addr -> pure addr

    saveProcAddr :: Text -> Addr -> CompilationM ()
    saveProcAddr name addr =
      modify
        ( \s ->
            s
              { compilationStateLabels = (name, addr) : compilationStateLabels s
              }
        )

    -- FIXME: proc must be defined before call
    compileHorth' :: CompilationM ()
    compileHorth' = do
      currAst :| restAst <- asks compilationEnvAst
      case currAst of
        AstPushLit lit -> do
          emit $ OpCodePushLit lit
          continueLinear restAst
        AstIntr intr -> do
          emit $ OpCodeIntr intr
          continueLinear restAst
        AstIf ifAst -> mdo
          emit $ OpCodeIntr Not
          emit $ OpCodePushLit (LitAddr addrAfterIfBranch)
          emit $ OpCodeIntr Jet
          case nonEmpty ifAst of
            Nothing -> pure ()
            Just ifAst' -> do
              labels <- gets compilationStateLabels
              local (\e -> e {compilationEnvAst = ifAst'}) compileHorth'
              modify (\s -> s {compilationStateLabels = labels})
          addrAfterIfBranch <- gets compilationStateNextAddr
          continueLinear restAst
        AstName name -> mdo
          emit $ OpCodePushToCallStack callStackRetAddr
          addr <- getProcAddr name
          emit $ OpCodePushLit (LitAddr addr)
          emit $ OpCodeIntr Jmp
          callStackRetAddr <- gets compilationStateNextAddr
          continueLinear restAst
        AstProc name _inType _outType procAst -> mdo
          emit $ OpCodePushLit (LitAddr addrAfterProc)
          emit $ OpCodeIntr Jmp
          procStartAddr <- gets compilationStateNextAddr
          saveProcAddr name procStartAddr
          case nonEmpty procAst of
            Nothing -> pure ()
            Just procAst' -> do
              labels <- gets compilationStateLabels
              local (\e -> e {compilationEnvAst = procAst'}) compileHorth'
              modify (\s -> s {compilationStateLabels = labels})
          emit OpCodePopJmpFromCallStack
          addrAfterProc <- gets compilationStateNextAddr
          continueLinear restAst

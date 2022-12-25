{-# LANGUAGE AllowAmbiguousTypes, RecursiveDo #-}

module Lib where

import Control.Monad.Fix (MonadFix)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Control.Monad.Trans.Class (lift)
import Unsafe.Coerce (unsafeCoerce)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Control.Monad.Error (MonadError, throwError)
import Data.String (fromString)
import Data.Either (fromRight)
import Control.Monad (void, unless, when)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Void (absurd)
import Data.Functor.Const (Const (getConst))
import Data.Functor.Identity (Identity (Identity))
import Control.Monad.Reader (MonadReader, Reader, ReaderT, runReader, runReaderT, asks, local)
import Control.Monad.State (MonadState, StateT, evalStateT, get, gets, modify, execStateT)
import qualified Data.Vector as V
import Debug.Trace (traceShowM)
import Text.Megaparsec (parseTest, parse)

import Types
import Parser

-- collectLabels :: [OpCodeWithLabels] -> [(Text, Addr)]
-- collectLabels = collectLabels' 0
--   where
--   collectLabels' :: Addr -> [OpCodeWithLabels] -> [(Text, Addr)]
--   collectLabels' _ [] = []
--   collectLabels' addr (LabelDecl (Identity label) : rest) =
--     (label, addr) : collectLabels' addr rest
--   collectLabels' addr (_ : rest) = collectLabels' (addr + 1) rest

-- -- TODO: Error handling
-- resolveLabels :: [OpCodeWithLabels] -> Code
-- resolveLabels c = Code $ V.fromList $ go c
--   where
--     labels = collectLabels c
--     -- TODO: Data.Map
--     go :: [OpCodeWithLabels] -> [OpCode]
--     go [] = []
--     go (op : ops) = case op of
--       LabelDecl (Identity _) -> go ops
--       LabelRef (Identity label) -> case lookup label labels of
--         Nothing -> error $ "Label not found: " <> show label
--         Just addr -> PushLit (LitAddr addr) : go ops
--       PushLit lit -> PushLit lit : go ops
--       Intr intr -> Intr intr : go ops
--       Name name -> Name name : go ops

interpret :: Code -> Stack
interpret = runReader (evalStateT (runMachine interpret') (MachineState (Stack []) 0 []))
  where
    isDone :: Machine Bool
    isDone = do
      len <- asks (V.length . getCode)
      pc <- gets pc
      pure $ getAddr pc >= fromIntegral len

    incrementPC :: Machine ()
    incrementPC = modify (\s -> s {pc = pc s + 1})

    currentOp :: Machine OpCode
    currentOp = do
      pc <- gets pc
      code <- asks getCode
      pure $ code V.! (getAddr pc)

    push :: Lit -> Machine ()
    push l = modify (\s -> s {stack = Stack (l : getStack (stack s))})

    pop :: Machine Lit
    pop = do
      Stack (a : rest) <- gets stack
      modify (\s -> s {stack = Stack rest})
      pure a

    interpret' :: Machine Stack
    interpret' = do

      -- do
      --   op <- currentOp
      --   st <- get
      --   traceShowM (show op <> " <- " <> show st)

      currentOp >>= \case
        OpCodePushLit lit -> do
          push lit
          incrementPC
        OpCodeIntr Add -> do
          LitInt a <- pop
          LitInt b <- pop
          push $ LitInt $ b + a
          incrementPC
        OpCodeIntr Sub -> do
          LitInt a <- pop
          LitInt b <- pop
          push $ LitInt $ b - a
          incrementPC
        OpCodeIntr Mul -> do
          LitInt a <- pop
          LitInt b <- pop
          push $ LitInt $ b * a
          incrementPC
        OpCodeIntr Div -> do
          LitInt a <- pop
          LitInt b <- pop
          push $ LitInt $ b `div` a
          incrementPC
        OpCodeIntr EqI -> do
          LitInt a <- pop
          LitInt b <- pop
          push $ LitBool (b == a)
          incrementPC
        OpCodeIntr Not -> do
          LitBool l <- pop
          push $ LitBool $ not l
          incrementPC
        OpCodeIntr Jmp -> do
          LitAddr addr <- pop
          modify (\s -> s {pc = addr})
        OpCodeIntr Jet -> do
          LitAddr addr <- pop
          LitBool cond <- pop
          if cond then modify (\s -> s {pc = addr}) else incrementPC
        OpCodeIntr Dup -> do
          l <- pop
          push l
          push l
          incrementPC
        OpCodeIntr Swap -> do
          a <- pop
          b <- pop
          push a
          push b
          incrementPC
        OpCodeIntr Pop -> do
          void $ pop
          incrementPC
        OpCodeIntr Over -> do
          a <- pop
          b <- pop
          push b
          push a
          push b
          incrementPC
        OpCodePushToCallStack addr -> do
          modify (\s -> s {callStack = addr : callStack s})
          incrementPC
        OpCodePopJmpFromCallStack -> do
          addr : rest <- gets callStack
          modify (\s -> s {callStack = rest})
          modify (\s -> s {pc = addr})

      done <- isDone
      if done then gets stack else interpret'

type TypeError = Text

data TypeCheckState = TypeCheckState
  { typeCheckStack :: [HType]
  , typeCheckPC :: Addr
  , typeCheckUsedTyVars :: Integer
  }

newtype TypeCheckMachine a = TypeCheckMachine
  { runTypeCheckMachine :: StateT TypeCheckState (ReaderT Code (Either TypeError)) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadState TypeCheckState, MonadReader Code, MonadError TypeError)

instance MonadFail TypeCheckMachine where
  fail = throwError . fromString

data Peano = Z | S Peano
  deriving stock (Eq, Show)

data Vec (n :: Peano) (a :: Type) where
  Nil :: Vec Z a
  (:>) :: a -> Vec n a -> Vec (S n) a
infixr 5 :>

deriving stock instance Show a => Show (Vec n a)

-- | Get runtime length of a vector
vecLength :: forall (n :: Peano) (a :: Type). Vec n a -> Int
vecLength Nil = 0
vecLength (_ :> rest) = 1 + vecLength rest

-- | Convert vector to list
vecToList :: forall (n :: Peano) (a :: Type). Vec n a -> [a]
vecToList Nil = []
vecToList (a :> rest) = a : vecToList rest

-- | Create a vector from list matching length of another vector
vecFromList :: forall (n :: Peano) (a :: Type) (b :: Type). [a] -> Vec n b -> Vec n a
vecFromList [] Nil = Nil
vecFromList (a : as) (_ :> rest) = a :> vecFromList as rest
vecFromList [] _ = error "vecFromList: List too short"
vecFromList _ Nil = error "vecFromList: List too long"

-- stacksEqual ::
--   forall (n :: Peano).
--   Vec n HType ->
--   Vec n HType ->
--   Bool
-- stacksEqual = stacksEqual' mempty
--   where
--     stacksEqual' :: forall (m :: Peano). [(Integer, HType)] -> Vec m HType -> Vec m HType -> Bool
--     stacksEqual' _ Nil Nil = True
--     stacksEqual' tyVars (HTypeVar tyIdx :> ts1) (t2 :> ts2) =
--       case lookup tyIdx tyVars of
--         Nothing -> stacksEqual' ((tyIdx, t2) : tyVars) ts1 ts2
--         Just t1 -> t1 == t2 && stacksEqual' tyVars ts1 ts2
--     stacksEqual' tyVars (t1 :> ts1) (t2 :> ts2) =
--       t1 == t2 && stacksEqual' tyVars ts1 ts2

-- typeCheck :: Code -> Either TypeError [HType]
-- typeCheck c =
--   flip runReaderT c
--   . flip evalStateT (TypeCheckState [] 0 0)
--   . runTypeCheckMachine $ go
--   where
--     currentOp :: TypeCheckMachine OpCode
--     currentOp = do
--       pc <- gets typeCheckPC
--       code <- asks getCode
--       pure $ code V.! (getAddr pc)

--     isDone :: TypeCheckMachine Bool
--     isDone = do
--       len <- asks (V.length . getCode)
--       pc <- gets typeCheckPC
--       pure $ getAddr pc >= fromIntegral len

--     incrementPC :: TypeCheckMachine ()
--     incrementPC = modify (\s -> s {typeCheckPC = typeCheckPC s + 1})

--     pushType :: HType -> TypeCheckMachine ()
--     pushType (HTypeVar _) = error "pushType: Implementation bug - Type variable pushed"
--     pushType t = modify (\s -> s {typeCheckStack = t : typeCheckStack s})

--     popTypes ::
--       forall (n :: Peano).
--       Vec n HType ->
--       TypeCheckMachine (Vec n HType)
--     popTypes t = do
--       op <- currentOp
--       let expectedLen = vecLength t
--       stackLst <- gets ((take expectedLen) . typeCheckStack)
--       let err = throwError $ Text.pack $ mconcat
--                   [ "Stack top mismatch, expected: "
--                   , show $ vecToList t
--                   , ", got: "
--                   , show stackLst
--                   , ".\n"
--                   , "While checking: "
--                   , show op
--                   ]
--       when (length stackLst /= expectedLen) err
--       -- 'vecFromList' is safe here because we checked the length of the list
--       let stack :: Vec n HType = vecFromList stackLst t
--       unless (stacksEqual t stack) err  
--       modify (\s -> s {typeCheckStack = drop expectedLen (typeCheckStack s)})
--       pure stack

--     generateTyVar :: TypeCheckMachine HType
--     generateTyVar = do
--       n <- gets typeCheckUsedTyVars
--       modify (\s -> s {typeCheckUsedTyVars = typeCheckUsedTyVars s + 1})
--       pure $ HTypeVar n

--     go :: TypeCheckMachine [HType]
--     go = do
--       op <- currentOp
--       case op of
--         PushLit (LitInt _) -> do
--           pushType HInt
--           incrementPC
--         PushLit (LitBool _) -> do
--           pushType HBool
--           incrementPC
--         PushLit (LitAddr _) -> do
--           pushType HAddr
--           incrementPC
--         Intr Add -> do
--           void $ popTypes (HInt :> HInt :> Nil)
--           pushType HInt
--           incrementPC
--         Intr Sub -> do
--           void $ popTypes (HInt :> HInt :> Nil)
--           pushType HInt
--           incrementPC
--         Intr Mul -> do
--           void $ popTypes (HInt :> HInt :> Nil)
--           pushType HInt
--           incrementPC
--         Intr Div -> do
--           void $ popTypes (HInt :> HInt :> Nil)
--           pushType HInt
--           incrementPC
--         Intr EqI -> do
--           void $ popTypes (HInt :> HInt :> Nil)
--           pushType HBool
--           incrementPC
--         Intr Not -> do
--           void $ popTypes (HBool :> Nil)
--           pushType HBool
--           incrementPC
--         Intr Swap -> do
--           a <- generateTyVar
--           b <- generateTyVar
--           (a' :> b' :> Nil) <- popTypes (a :> b :> Nil)
--           pushType a'
--           pushType b'
--           incrementPC
--         Intr Dup -> do
--           a <- generateTyVar
--           (a' :> Nil) <- popTypes (a :> Nil)
--           pushType a'
--           pushType a'
--           incrementPC
--         Intr Pop -> do
--           a <- generateTyVar
--           void $ popTypes (a :> Nil)
--           incrementPC
--         Intr Over -> do
--           a <- generateTyVar
--           b <- generateTyVar
--           (a' :> b' :> Nil) <- popTypes (a :> b :> Nil)
--           pushType b'
--           pushType a'
--           pushType b'
--           incrementPC
        
--         -- TODO: Jumps
--         -- Intr Jet -> do
--         --   popTypes [HAddr, HBool]
--         --   incrementPC
--         LabelDecl label -> absurd $ getConst label
--         LabelRef label -> absurd $ getConst label
--         Name _ -> throwError "Name not implemented"
          
--       done <- isDone
--       if done then gets typeCheckStack else go

data CompileError
  = CompileError Text
  deriving stock (Show, Eq)

data CompilationState = CompilationState
  { compilationStateEmited :: [OpCode]
  , compilationStateNextAddr :: Addr
  , compilationStateLabels :: [(Text, Addr)]
  } deriving stock (Show, Eq)

data CompilationEnv = CompilationEnv
  { compilationEnvAst :: NonEmpty Ast
  } deriving stock (Show, Eq)

newtype CompilationM a = CompilationM
  { runCompilationM :: (StateT CompilationState (ReaderT CompilationEnv (Either CompileError))) a
  } deriving newtype (Functor, Applicative, Monad, MonadReader CompilationEnv, MonadState CompilationState, MonadError CompileError, MonadFix)

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
    emit op = modify (\s -> s
                       { compilationStateEmited = op : compilationStateEmited s
                       , compilationStateNextAddr = compilationStateNextAddr s + 1
                       })

    -- Continue compiling rest of ast in a block
    continueLinear :: [Ast] -> CompilationM ()
    continueLinear [] = pure ()
    continueLinear (a : as) = local (\e -> e {compilationEnvAst = a :| as}) compileHorth'

    -- TODO: Location
    getProcAddr :: Text -> CompilationM Addr
    getProcAddr name = do
      labels <- gets compilationStateLabels
      case lookup name labels of
        Nothing -> throwError $ CompileError $ Text.pack $ mconcat
          [ "Unknown procedure: "
          , show name
          ]
        Just addr -> pure addr

    saveProcAddr :: Text -> Addr -> CompilationM ()
    saveProcAddr name addr = modify (\s -> s
      { compilationStateLabels = (name, addr) : compilationStateLabels s
      })

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

main :: IO ()
main = do
  putStrLn ""
  case compileHorth fac_go of
    Left (CompileError err) -> Text.putStrLn err
    Right opCodes -> do
      mapM_ (\(i, op) -> Text.putStrLn (prettyAddr (Addr i) <> ": " <> prettyOpCode op)) $ zip [0..] $ V.toList $ getCode opCodes
      print $ interpret opCodes


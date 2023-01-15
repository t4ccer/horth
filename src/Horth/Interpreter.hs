module Horth.Interpreter (interpret) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT, gets, modify)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Word (Word8)

import Horth.Compiler (Code (getCode))
import Horth.Types

data MachineState = MachineState
  { stack :: Stack
  , pc :: Addr
  , callStack :: [Addr]
  , memory :: V.Vector Word8
  }
  deriving stock (Show, Eq)

newtype Machine a = Machine {runMachine :: StateT MachineState (ReaderT Code IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState MachineState, MonadReader Code)

instance MonadFail Machine where
  -- NOTE: Machine should never need to fail after typechecking
  fail s = error $ "fail(Machine): " <> s <> ". This is a bug."

interpret :: Code -> IO Stack
interpret =
  runReaderT
    . flip evalStateT (MachineState (Stack []) 0 [] (V.replicate 640_000 0)) -- Ought to be enough for anybody
    $ runMachine interpret'
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

    -- Add string to memory and return address
    addStrToMem :: ByteString -> Machine Int64
    addStrToMem str = do
      mem <- gets memory
      let startPtr = V.length mem
      let mem' = mem <> V.fromList (BS.unpack str <> [0])
      modify (\s -> s {memory = mem'})
      pure $ fromIntegral startPtr

    interpret' :: Machine Stack
    interpret' = do
      currentOp >>= \case
        OpCodePushLit (LitStr str) -> do
          ptr <- addStrToMem str
          push $ LitPtr ptr
          incrementPC
        OpCodePushLit lit -> do
          push lit
          incrementPC
        OpCodeIntr Add -> do
          LitInt a <- pop
          b' <- pop
          case b' of
            LitInt b -> push $ LitInt $ b + a
            LitPtr b -> push $ LitPtr $ b + a
            _ -> error "OpCodeIntr: Add: invalid type"
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
        OpCodeIntr (Jmp addr) -> do
          modify (\s -> s {pc = addr})
        OpCodeIntr (Jet addr) -> do
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
        OpCodeIntr PrintI -> do
          LitInt a <- pop
          liftIO $ print a
          incrementPC
        OpCodeIntr PrintB -> do
          LitBool a <- pop
          liftIO $ print a
          incrementPC
        OpCodeIntr PrintS -> do
          LitInt len <- pop
          LitPtr ptr <- pop
          mem <- gets memory
          liftIO $ BS.putStr $ BS.pack $ V.toList $ V.take (fromIntegral len) $ V.drop (fromIntegral ptr) mem
          incrementPC
        OpCodeIntr Read1 -> do
          LitPtr ptr <- pop
          mem <- gets memory
          push $ LitInt $ fromIntegral $ mem V.! (fromIntegral ptr)
          incrementPC
        OpCodeIntr Write1 -> do
          LitPtr ptr <- pop
          LitInt val <- pop
          -- TODO: Be smart about 'fromIntegral' here
          modify (\s -> s {memory = V.modify (\v -> MV.write v (fromIntegral ptr) (fromIntegral val)) $ memory s})
          incrementPC
        OpCodeIntr Mem -> do
          push $ LitPtr 0
          incrementPC
        OpCodePushToCallStack retAddr callAddr -> do
          modify (\s -> s {callStack = retAddr : callStack s})
          modify (\s -> s {pc = callAddr})
        OpCodePopJmpFromCallStack -> do
          addr : rest <- gets callStack
          modify (\s -> s {callStack = rest})
          modify (\s -> s {pc = addr})

      done <- isDone
      if done then gets stack else interpret'

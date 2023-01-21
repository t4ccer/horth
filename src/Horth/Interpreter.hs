module Horth.Interpreter (interpret) where

import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT, gets, modify)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Vector qualified as V
import Data.Vector.Unboxed.Mutable qualified as UMV
import Data.Word (Word8)

import Horth.Compiler (Code (getCode))
import Horth.Types

data MachineState = MachineState
  { stack :: Stack
  , pc :: Addr
  , callStack :: [Addr]
  , memory :: UMV.IOVector Word8
  , strings :: Map Addr Int64
  , notAllocated :: Int64
  }

newtype Machine a = Machine {runMachine :: StateT MachineState (ReaderT Code IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState MachineState, MonadReader Code)

instance MonadFail Machine where
  -- NOTE: Machine should never need to fail after typechecking
  fail s = error $ "fail(Machine): " <> s <> ". This is a bug."

interpret :: Code -> IO Stack
interpret code =
  do
    -- Ought to be enough for anybody
    memVec <- UMV.replicate 640_000 0
    flip runReaderT code
      . flip evalStateT (MachineState (Stack []) 0 [] memVec mempty 0)
      $ runMachine
      $ do
        emitStrings
        interpret'
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
      let c = code V.! (getAddr pc)
      pure c

    push :: Lit -> Machine ()
    push l = modify (\s -> s {stack = Stack (l : getStack (stack s))})

    pop :: Machine Lit
    pop = do
      Stack (a : rest) <- gets stack
      modify (\s -> s {stack = Stack rest})
      pure a

    -- Add string to memory and return address
    addStrToMem :: ByteString -> Machine ()
    addStrToMem str = do
      pc <- gets pc
      ptr <- gets notAllocated
      mem <- gets memory
      let bytes = BS.unpack str
      forM_ (zip [0 ..] bytes) $ \(offset, byte) -> do
        liftIO $ UMV.write mem (fromIntegral ptr + offset) byte
      modify
        ( \s ->
            s
              { notAllocated = ptr + fromIntegral (BS.length str) + 2
              , strings = Map.insert pc ptr (strings s)
              }
        )

    emitStrings :: Machine ()
    emitStrings = do
      currentOp >>= \case
        OpCodePushLit (LitStr str) -> addStrToMem str
        _ -> pure ()
      incrementPC
      done <- isDone
      if done
        then modify (\s -> s {pc = 0})
        else emitStrings

    interpret' :: Machine Stack
    interpret' = do
      currentOp >>= \case
        OpCodePushLit (LitStr _) -> do
          strs <- gets strings
          pc' <- gets pc
          push $ LitPtr $ (strs Map.! pc')
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
          let strMem = UMV.slice (fromIntegral ptr) (fromIntegral len) mem
          str <- liftIO $ mvecToList strMem
          liftIO $ BS.putStr $ BS.pack str
          incrementPC
        OpCodeIntr Read1 -> do
          LitPtr ptr <- pop
          mem <- gets memory
          ptrVal <- liftIO $ UMV.read mem (fromIntegral ptr)
          push $ LitInt $ fromIntegral ptrVal
          incrementPC
        OpCodeIntr Write1 -> do
          LitPtr ptr <- pop
          LitInt val <- pop
          -- TODO: Be smart about 'fromIntegral' here
          mem <- gets memory
          liftIO $ UMV.write mem (fromIntegral ptr) (fromIntegral val)
          incrementPC
        OpCodeIntr Mem -> do
          addr <- gets notAllocated
          push $ LitPtr addr
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

mvecToList :: forall (a :: Type). UMV.Unbox a => UMV.IOVector a -> IO [a]
mvecToList mvec = reverse <$> UMV.foldl' (\acc i -> i : acc) [] mvec

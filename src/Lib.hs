module Lib (main) where

import Control.Monad (void)
import Control.Monad.Reader (asks, runReader)
import Control.Monad.State (evalStateT, gets, modify)
import Data.Text.IO qualified as Text
import Data.Vector qualified as V
import Text.Megaparsec ()

import Compiler
import Parser ()
import TypeChecker
import Types

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

main :: IO ()
main = do
  putStrLn ""
  case typeCheck fac_go of
    Left err -> Text.putStrLn err
    Right (ast, ty) -> do
      print ty
      case compileHorth ast of
        Left (CompileError err) -> Text.putStrLn err
        Right opCodes -> do
          -- mapM_ (\(i, op) -> Text.putStrLn (prettyAddr (Addr i) <> ": " <> prettyOpCode op)) $ zip [0..] $ V.toList $ getCode opCodes
          Text.putStrLn ""
          print $ opCodes
          Text.putStrLn ""
          print $ interpret opCodes

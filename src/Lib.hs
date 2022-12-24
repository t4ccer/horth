module Lib where

import Control.Monad.Error (MonadError, throwError)
import Data.String (fromString)
import Data.Either (fromRight)
import Control.Monad (void, unless)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Void (absurd)
import Data.Functor.Const (Const (getConst))
import Data.Functor.Identity (Identity (Identity))
import Control.Monad.Reader (MonadReader, Reader, ReaderT, runReader, runReaderT, asks)
import Control.Monad.State (MonadState, StateT, evalStateT, get, gets, modify)
import qualified Data.Vector as V
import Debug.Trace (traceShowM)
import Text.Megaparsec (parseTest, parse)

import Types
import Parser

collectLabels :: [OpCodeWithLabels] -> [(Text, Addr)]
collectLabels = collectLabels' 0
  where
  collectLabels' :: Addr -> [OpCodeWithLabels] -> [(Text, Addr)]
  collectLabels' _ [] = []
  collectLabels' addr (LabelDecl (Identity label) : rest) =
    (label, addr) : collectLabels' addr rest
  collectLabels' addr (_ : rest) = collectLabels' (addr + 1) rest

-- TODO: Error handling
resolveLabels :: [OpCodeWithLabels] -> Code
resolveLabels c = Code $ V.fromList $ go c
  where
    labels = collectLabels c
    -- TODO: Data.Map
    go :: [OpCodeWithLabels] -> [OpCode]
    go [] = []
    go (op : ops) = case op of
      LabelDecl (Identity _) -> go ops
      LabelRef (Identity label) -> case lookup label labels of
        Nothing -> error $ "Label not found: " <> show label
        Just addr -> PushLit (LitAddr addr) : go ops
      PushLit lit -> PushLit lit : go ops
      Intr intr -> Intr intr : go ops
      Name name -> Name name : go ops

interpret :: Code -> Stack
interpret = runReader (evalStateT (runMachine interpret') (MachineState (Stack []) 0))
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
        PushLit lit -> do
          push lit
          incrementPC
        Intr Add -> do
          LitInt a <- pop
          LitInt b <- pop
          push $ LitInt $ b + a
          incrementPC
        Intr Sub -> do
          LitInt a <- pop
          LitInt b <- pop
          push $ LitInt $ b - a
          incrementPC
        Intr Mul -> do
          LitInt a <- pop
          LitInt b <- pop
          push $ LitInt $ b * a
          incrementPC
        Intr Div -> do
          LitInt a <- pop
          LitInt b <- pop
          push $ LitInt $ b `div` a
          incrementPC
        Intr EqI -> do
          LitInt a <- pop
          LitInt b <- pop
          push $ LitBool (b == a)
          incrementPC
        Intr Not -> do
          LitBool l <- pop
          push $ LitBool $ not l
          incrementPC
        Intr Jet -> do
          LitAddr addr <- pop
          LitBool cond <- pop
          if cond then modify (\s -> s {pc = addr}) else incrementPC
        Intr Dup -> do
          l <- pop
          push l
          push l
          incrementPC
        Intr Swap -> do
          a <- pop
          b <- pop
          push a
          push b
          incrementPC
        Intr Pop -> do
          void $ pop
          incrementPC
        Intr Over -> do
          a <- pop
          b <- pop
          push b
          push a
          push b
          incrementPC
        LabelDecl label -> absurd $ getConst label -- GHC can't see that this is unreachable
        LabelRef label -> absurd $ getConst label
        Name name -> error $ "Name not implemented: " <> show name

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

stacksEqual ::
  [HType] ->
  -- ^ Expected stack
  [HType] ->
  -- ^ Actual stack
  Bool
stacksEqual = stacksEqual' mempty
  where
    stacksEqual' :: [(Integer, HType)] -> [HType] -> [HType] -> Bool
    stacksEqual' _ [] [] = True
    stacksEqual' _ [] _ = False
    stacksEqual' _ _ [] = False
    stacksEqual' tyVars (HTypeVar tyIdx : ts1) (t2 : ts2) =
      case lookup tyIdx tyVars of
        Nothing -> stacksEqual' ((tyIdx, t2) : tyVars) ts1 ts2
        Just t1 -> t1 == t2 && stacksEqual' tyVars ts1 ts2
    stacksEqual' tyVars (t1 : ts1) (t2 : ts2) =
      t1 == t2 && stacksEqual' tyVars ts1 ts2

typeCheck :: Code -> Either TypeError [HType]
typeCheck c =
  flip runReaderT c
  . flip evalStateT (TypeCheckState [] 0 0)
  . runTypeCheckMachine $ go
  where
    currentOp :: TypeCheckMachine OpCode
    currentOp = do
      pc <- gets typeCheckPC
      code <- asks getCode
      pure $ code V.! (getAddr pc)

    isDone :: TypeCheckMachine Bool
    isDone = do
      len <- asks (V.length . getCode)
      pc <- gets typeCheckPC
      pure $ getAddr pc >= fromIntegral len

    incrementPC :: TypeCheckMachine ()
    incrementPC = modify (\s -> s {typeCheckPC = typeCheckPC s + 1})

    pushType :: HType -> TypeCheckMachine ()
    pushType (HTypeVar _) = error "pushType: Implementation bug - Type variable pushed"
    pushType t = modify (\s -> s {typeCheckStack = t : typeCheckStack s})

    -- TODO: Use type level vec
    popTypes :: [HType] -> TypeCheckMachine [HType]
    popTypes t = do
      stack <- gets (take (length t) . typeCheckStack)
      unless (stacksEqual t stack) $ do
        op <- currentOp
        throwError $ Text.pack $ mconcat
          [ "Stack top mismatch, expected: "
          , show t
          , ", got: "
          , show stack
          , ".\n"
          , "While checking: "
          , show op
          ]
      modify (\s -> s {typeCheckStack = drop (length t) (typeCheckStack s)})
      pure stack

    generateTyVar :: TypeCheckMachine HType
    generateTyVar = do
      n <- gets typeCheckUsedTyVars
      modify (\s -> s {typeCheckUsedTyVars = typeCheckUsedTyVars s + 1})
      pure $ HTypeVar n

    go :: TypeCheckMachine [HType]
    go = do
      op <- currentOp
      case op of
        PushLit (LitInt _) -> do
          pushType HInt
          incrementPC
        PushLit (LitBool _) -> do
          pushType HBool
          incrementPC
        PushLit (LitAddr _) -> do
          pushType HAddr
          incrementPC
        Intr Add -> do
          void $ popTypes [HInt, HInt]
          pushType HInt
          incrementPC
        Intr Sub -> do
          void $ popTypes [HInt, HInt]
          pushType HInt
          incrementPC
        Intr Mul -> do
          void $ popTypes [HInt, HInt]
          pushType HInt
          incrementPC
        Intr Div -> do
          void $ popTypes [HInt, HInt]
          pushType HInt
          incrementPC
        Intr EqI -> do
          void $ popTypes [HInt, HInt]
          pushType HBool
          incrementPC
        Intr Not -> do
          void $ popTypes [HBool]
          pushType HBool
          incrementPC
        Intr Swap -> do
          a <- generateTyVar
          b <- generateTyVar
          [a', b'] <- popTypes [a, b]
          pushType a'
          pushType b'
          incrementPC
        Intr Dup -> do
          a <- generateTyVar
          [a'] <- popTypes [a]
          pushType a'
          pushType a'
          incrementPC
        Intr Pop -> do
          a <- generateTyVar
          void $ popTypes [a]
          incrementPC
        Intr Over -> do
          a <- generateTyVar
          b <- generateTyVar
          [a', b'] <- popTypes [a, b]
          pushType b'
          pushType a'
          pushType b'
          incrementPC
        -- TODO: Jumps
        -- Intr Jet -> do
        --   popTypes [HAddr, HBool]
        --   incrementPC
        LabelDecl label -> absurd $ getConst label
        LabelRef label -> absurd $ getConst label
        Name _ -> throwError "Name not implemented"
          
      done <- isDone
      if done then gets typeCheckStack else go

main :: IO ()
main = do
  let testCode = resolveLabels $ fromRight undefined $ parse horthParser "<none>" "1 2 over add add"
  putStrLn ""
  print testCode
  putStrLn ""
  case typeCheck testCode of
    Left err -> Text.putStrLn err
    Right types -> do
      print types
      putStrLn ""
      print $ interpret testCode

  
  -- parseTest horthParser "3 loop: 1 sub dup 0 eqi !end jet true !loop jet end:"
  -- testCode3 <- case parse horthParser "<none>" "3 loop: 1 sub dup 0 == !end jet true !loop jet end" of
  --   Left err -> error $ show err
  --   Right code -> pure code
  -- traceShowM testCode3
  -- traceShowM $ resolveLabels testCode3
  -- print $ interpret $ resolveLabels testCode3

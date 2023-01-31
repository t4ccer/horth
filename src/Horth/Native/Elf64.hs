module Horth.Native.Elf64 (compileElf64) where

import Control.Monad.State (MonadState, State, execState, gets, modify)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder qualified as TextBuilder
import Data.Vector qualified as Vector

import Horth.Compiler (Code (getCode))
import Horth.Types

data CompilationState = CompilationState
  { compilationStateEmited :: TextBuilder.Builder
  , compilationStateCode :: [OpCode]
  , compilationStateIp :: Addr
  }
  deriving stock (Show)

newtype CompilationM a = CompilationM {runCompilationM :: State CompilationState a}
  deriving newtype (Functor, Applicative, Monad, MonadState CompilationState)

compileElf64 :: Code -> Text
compileElf64 code =
  toStrict
    . TextBuilder.toLazyText
    . compilationStateEmited
    . flip execState (CompilationState mempty (Vector.toList $ getCode code) (Addr 0))
    . runCompilationM
    $ do
      prologue
      compileOpCode
      epilogue
  where
    compileOpCode :: CompilationM ()
    compileOpCode = do
      opCodes <- gets compilationStateCode
      (Addr currAddr) <- gets compilationStateIp
      let mkAddrPostfix txt = txt <> "_" <> Text.pack (show currAddr)
      case opCodes of
        [] -> pure ()
        (opCode : rest) -> do
          modify $ \s -> s {compilationStateCode = rest}
          emitComment $ Text.pack $ show opCode
          emitIpLabel
          case opCode of
            OpCodePushLit (LitInt i) -> do
              emitInstr "push" [Text.pack $ show i]
            OpCodePushLit (LitPtr _) -> do
              error "compileOpCode: OpCodePushLit (LitPtr _)"
            OpCodePushLit (LitBool True) -> do
              emitInstr "push" ["1"]
            OpCodePushLit (LitBool False) -> do
              emitInstr "push" ["0"]
            OpCodePushLit (LitStr _) -> do
              emitInstr "push" ["str_" <> Text.pack (show currAddr)]
            OpCodeIntr Add -> do
              emitInstr "pop" ["r13"]
              emitInstr "add" ["[rsp]", "r13"]
            OpCodeIntr Sub -> do
              emitInstr "pop" ["r13"]
              emitInstr "sub" ["[rsp]", "r13"]
            OpCodeIntr Mul -> do
              emitInstr "pop" ["r13"]
              emitInstr "pop" ["r12"]
              emitInstr "imul" ["r12", "r13"]
              emitInstr "push" ["r12"]
            OpCodeIntr Div -> do
              emitInstr "mov" ["rdx", "0"]
              emitInstr "pop" ["rcx"]
              emitInstr "pop" ["rax"]
              emitInstr "idiv" ["rcx"]
              emitInstr "push" ["rax"]
            OpCodeIntr Not -> do
              emitInstr "pop" ["r13"]
              emitInstr "mov" ["r12", "1"]
              emitInstr "sub" ["r12", "r13"]
              emitInstr "push" ["r12"]
            OpCodeIntr Dup -> do
              emitInstr "push qword" ["[rsp]"]
            OpCodeIntr Swap -> do
              emitInstr "mov" ["r13", "qword [rsp]"]
              emitInstr "xchg" ["r13", "qword [rsp+8]"]
              emitInstr "mov" ["[rsp]", "r13"]
            OpCodeIntr Over -> do
              emitInstr "push qword" ["[rsp+8]"]
            OpCodeIntr Rot -> do
              -- TODO: Optimize?
              emitInstr "pop" ["r13"]
              emitInstr "pop" ["r12"]
              emitInstr "pop" ["r11"]
              emitInstr "push" ["r13"]
              emitInstr "push" ["r11"]
              emitInstr "push" ["r12"]
            OpCodeIntr Pop -> do
              emitInstr "add" ["rsp", "8"]
            OpCodeIntr EqI -> do
              -- TODO: There must be a smarter way
              emitInstr "pop" ["r13"]
              emitInstr "pop" ["r12"]
              emitInstr "cmp" ["r13", "r12"]
              emitInstr "je" [mkAddrPostfix "eq"]
              emitInstr "jmp" [mkAddrPostfix "neq"]
              emitLabel $ mkAddrPostfix "eq"
              emitInstr "push" ["1"]
              emitInstr "jmp" [mkAddrPostfix "end"]
              emitLabel $ mkAddrPostfix "neq"
              emitInstr "push" ["0"]
              emitLabel $ mkAddrPostfix "end"
            OpCodeIntr (Jmp (Addr jmpAddr)) -> do
              emitInstr "jmp" ["ip_" <> Text.pack (show jmpAddr)]
            OpCodeIntr (Jet (Addr jmpAddr)) -> do
              emitInstr "pop" ["r13"]
              emitInstr "cmp" ["r13", "1"]
              emitInstr "je" ["ip_" <> Text.pack (show jmpAddr)]
            OpCodeIntr Read1 -> do
              emitInstr "pop " ["r13"]
              emitInstr "mov" ["r12", "[r13]"]
              emitInstr "and" ["r12", "0xff"]
              emitInstr "push" ["r12"]
            OpCodeIntr Read8 -> do
              emitInstr "pop " ["r13"]
              emitInstr "mov" ["r12", "[r13]"]
              emitInstr "push" ["r12"]
            OpCodeIntr Write1 -> do
              emitInstr "pop" ["r13"]
              emitInstr "pop" ["r12"]
              emitInstr "mov" ["[r13]", "r12b"]
            OpCodeIntr Write8 -> do
              emitInstr "pop" ["r13"]
              emitInstr "pop" ["r12"]
              emitInstr "mov" ["[r13]", "r12"]
            OpCodeIntr Mem -> do
              emitInstr "push" ["mem"]
            OpCodeIntr SysCall0 -> do
              emitInstr "pop" ["rax"]
              emitInstr "syscall" []
              emitInstr "push" ["rax"]
            OpCodeIntr SysCall1 -> do
              emitInstr "pop" ["rax"]
              emitInstr "pop" ["rdi"]
              emitInstr "syscall" []
              emitInstr "push" ["rax"]
            OpCodeIntr SysCall2 -> do
              emitInstr "pop" ["rax"]
              emitInstr "pop" ["rdi"]
              emitInstr "pop" ["rsi"]
              emitInstr "syscall" []
              emitInstr "push" ["rax"]
            OpCodeIntr SysCall3 -> do
              emitInstr "pop" ["rax"]
              emitInstr "pop" ["rdi"]
              emitInstr "pop" ["rsi"]
              emitInstr "pop" ["rdx"]
              emitInstr "syscall" []
              emitInstr "push" ["rax"]
            OpCodeIntr SysCall4 -> do
              emitInstr "pop" ["rax"]
              emitInstr "pop" ["rdi"]
              emitInstr "pop" ["rsi"]
              emitInstr "pop" ["rdx"]
              emitInstr "pop" ["r10"]
              emitInstr "syscall" []
              emitInstr "push" ["rax"]
            OpCodeIntr SysCall5 -> do
              emitInstr "pop" ["rax"]
              emitInstr "pop" ["rdi"]
              emitInstr "pop" ["rsi"]
              emitInstr "pop" ["rdx"]
              emitInstr "pop" ["r10"]
              emitInstr "pop" ["r8"]
              emitInstr "syscall" []
              emitInstr "push" ["rax"]
            OpCodeIntr SysCall6 -> do
              emitInstr "pop" ["rax"]
              emitInstr "pop" ["rdi"]
              emitInstr "pop" ["rsi"]
              emitInstr "pop" ["rdx"]
              emitInstr "pop" ["r10"]
              emitInstr "pop" ["r8"]
              emitInstr "pop" ["r9"]
              emitInstr "syscall" []
              emitInstr "push" ["rax"]
            OpCodeIntr UnsafeMkPtr -> pure ()
            OpCodeIntr (Rename _) -> pure ()
            OpCodePushToCallStack (Addr retAddr) (Addr jmpAddr) -> do
              emitInstr "add" ["r15", "8"]
              emitInstr "mov" ["qword [r15]", "ip_" <> Text.pack (show retAddr)]
              emitInstr "jmp" ["ip_" <> Text.pack (show jmpAddr)]
            OpCodePopJmpFromCallStack -> do
              emitInstr "mov" ["r14", "[r15]"]
              emitInstr "sub" ["r15", "8"]
              emitInstr "jmp" ["r14"]
          emitVerbatim ""
          modify $ \s -> s {compilationStateIp = Addr (currAddr + 1)}
          compileOpCode

emitComment :: Text -> CompilationM ()
emitComment comment = emitVerbatim $ "; " <> comment

emitInstr :: Text -> [Text] -> CompilationM ()
emitInstr opCode args = emitVerbatim $ "        " <> opCode <> " " <> Text.intercalate ", " args

emitIpLabel :: CompilationM ()
emitIpLabel = do
  (Addr currAddr) <- gets compilationStateIp
  emitLabel $ Text.pack $ "ip_" <> show currAddr

emitLabel :: Text -> CompilationM ()
emitLabel label = emitVerbatim (label <> ":")

emitSection :: Text -> CompilationM ()
emitSection section = emitVerbatim ("section " <> section)

emitGlobal :: Text -> CompilationM ()
emitGlobal global = emitVerbatim ("global " <> global)

emitVerbatim :: Text -> CompilationM ()
emitVerbatim text = do
  modify $ \s -> s {compilationStateEmited = compilationStateEmited s <> TextBuilder.fromText text <> "\n"}

epilogue :: CompilationM ()
epilogue = do
  emitComment "Epilogue"
  emitIpLabel
  emitInstr "mov" ["rax", "60"]
  emitInstr "mov" ["rdi", "0"]
  emitInstr "syscall" []

stringLits :: CompilationM [(Int, ByteString)]
stringLits = do
  opCodes <- gets compilationStateCode
  let getStr (ip, op)
        | OpCodePushLit (LitStr str) <- op = Just (ip, str)
        | otherwise = Nothing
  pure $ mapMaybe getStr $ zip [0 :: Int ..] opCodes

emitStr :: (Int, ByteString) -> CompilationM ()
emitStr (ip, str) = do
  emitVerbatim $
    mconcat
      [ "        str_"
      , Text.pack (show ip)
      , " db "
      , Text.pack (show str)
      , ", 0"
      ]

prologue :: CompilationM ()
prologue = do
  strings <- stringLits

  emitGlobal "_start"
  emitSection ".data"
  emitVerbatim "        nl db 0x0a"
  mapM_ emitStr strings
  emitSection ".bss"
  -- +1 is for null byte. TODO: Escape strings
  emitVerbatim ("        mem resq " <> Text.pack (show (640_000 - sum (fmap ((+ 1) . BS.length . snd) strings))))
  emitVerbatim "        horthCallStack resq 1024"

  emitSection ".text"
  emitLabel "_start"
  emitInstr "mov" ["r15", "horthCallStack"]
  emitInstr "pop" ["r14"]
  emitInstr "push qword" ["rsp"]
  emitInstr "push" ["r14"]

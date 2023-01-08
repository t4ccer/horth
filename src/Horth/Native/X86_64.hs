module Horth.Native.X86_64 (compileX86_64) where

import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadReader, Reader, asks, local, runReader)
import Control.Monad.State (MonadState, StateT, execStateT, gets, modify)
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as V

import Horth.Types

data CompilationState = CompilationState
  { compilationStateEmited :: [Text]
  }
  deriving stock (Show, Eq)

data CompilationEnv = CompilationEnv
  { compilationEnvAst :: NonEmpty OpCode
  }
  deriving stock (Show, Eq)

newtype CompilationM a = CompilationM {runCompilationM :: (StateT CompilationState (Reader CompilationEnv)) a}

compileX86_64 :: Code -> Text
compileX86_64 (V.toList . getCode -> code) =
  mconcat
    [ prologue
    , fold $ zipWith (\ip op -> "ip_" <> Text.pack (show ip) <> ":\n" <> emitInstr (Addr ip) op) [0 :: Int ..] code
    , epilogue
    ]

epilogue :: Text
epilogue =
  joinLines
    [ ";; Epilogue"
    , "mov rax, 60"
    , "mov rdi, 0"
    , "syscall"
    ]

prologue :: Text
prologue =
  joinLines
    [ "global _start"
    , "section .bss"
    , "horthCallStack resq 1024"
    , "section .data"
    , "nl db 0x0a"
    , "True db \"True\", 0x0a"
    , "False db \"False\", 0x0a"
    , "section .text"
    , "print_uint:"
    , "mov rax, rdi"
    , "xor rcx, rcx"
    , "mov r8, 10"
    , -- Push digits onto stack
      ".loop:"
    , "xor rdx, rdx"
    , "div r8"
    , "add dl, 0x30"
    , "dec rsp"
    , "mov [rsp], dl"
    , "inc rcx"
    , "test rax, rax"
    , "jnz .loop"
    , -- Print elements from stack
      "xor rax, rax"
    , "mov rsi, rsp"
    , "mov rdx, rcx"
    , "push rcx"
    , "mov rax, 1"
    , "mov rdi, 1"
    , "syscall"
    , "pop rcx"
    , "add rsp, rcx"
    , "mov rax, 1"
    , "mov rdi, 1"
    , "mov rsi, nl"
    , "mov rdx, 1"
    , "syscall"
    , "ret"
    , "_start:"
    , "mov r15, horthCallStack"
    ]

emitInstr :: Addr -> OpCode -> Text
emitInstr _ op@(OpCodePushLit (LitInt i)) =
  joinLines
    [ ";; " <> Text.pack (show op)
    , "push " <> Text.pack (show i)
    ]
emitInstr _ op@(OpCodePushLit (LitBool True)) =
  joinLines
    [ ";; " <> Text.pack (show op)
    , "push 1"
    ]
emitInstr _ op@(OpCodePushLit (LitBool False)) =
  joinLines
    [ ";; " <> Text.pack (show op)
    , "push 0"
    ]
emitInstr _ op@(OpCodeIntr Add) =
  joinLines
    [ ";; " <> Text.pack (show op)
    , "pop r13"
    , "pop r12"
    , "add r12, r13"
    , "push r12"
    ]
emitInstr _ op@(OpCodeIntr Sub) =
  joinLines
    [ ";; " <> Text.pack (show op)
    , "pop r13"
    , "pop r12"
    , "sub r12, r13"
    , "push r12"
    ]
emitInstr _ op@(OpCodeIntr Div) =
  joinLines
    [ ";; " <> Text.pack (show op)
    , "mov rdx, 0"
    , "pop rcx"
    , "pop rax"
    , "idiv rcx"
    , "push rax"
    ]
emitInstr _ op@(OpCodeIntr Mul) =
  joinLines
    [ ";; " <> Text.pack (show op)
    , "pop r13"
    , "pop r12"
    , "imul r12, r13"
    , "push r12"
    ]
emitInstr _ op@(OpCodeIntr Not) =
  joinLines
    [ ";; " <> Text.pack (show op)
    , "pop r13"
    , "mov r12, 1"
    , "sub r12, r13"
    , "push r12"
    ]
emitInstr _ op@(OpCodeIntr Dup) =
  joinLines
    [ ";; " <> Text.pack (show op)
    , "pop r13"
    , "push r13"
    , "push r13"
    ]
emitInstr _ op@(OpCodeIntr Swap) =
  joinLines
    [ ";; " <> Text.pack (show op)
    , "pop r13"
    , "pop r12"
    , "push r13"
    , "push r12"
    ]
emitInstr _ op@(OpCodeIntr Over) =
  joinLines
    [ ";; " <> Text.pack (show op)
    , "pop r13"
    , "pop r12"
    , "push r12"
    , "push r13"
    , "push r12"
    ]
emitInstr _ op@(OpCodeIntr Pop) =
  joinLines
    [ ";; " <> Text.pack (show op)
    , "pop r13"
    ]
emitInstr (Addr addr) op@(OpCodeIntr EqI) =
  joinLines
    [ ";; " <> Text.pack (show op)
    , "pop rdi"
    , "pop rsi"
    , "cmp rdi, rsi"
    , "je eq_" <> Text.pack (show addr)
    , "jmp neq_" <> Text.pack (show addr)
    , "eq_" <> Text.pack (show addr) <> ":"
    , "mov rax, 1"
    , "jmp end_" <> Text.pack (show addr)
    , "neq_" <> Text.pack (show addr) <> ":"
    , "mov rax, 0"
    , "end_" <> Text.pack (show addr) <> ":"
    , "push rax"
    ]
emitInstr _ op@(OpCodeIntr (Jmp (Addr addr))) =
  joinLines
    [ ";; " <> Text.pack (show op)
    , "jmp ip_" <> Text.pack (show addr)
    ]
emitInstr _ op@(OpCodeIntr PrintI) =
  joinLines
    [ ";; " <> Text.pack (show op)
    , "pop rdi"
    , "call print_uint"
    ]
emitInstr (Addr addr) op@(OpCodeIntr PrintB) =
  joinLines
    [ ";; " <> Text.pack (show op)
    , "mov rax, 1"
    , "mov rdi, 1"
    , "pop r10"
    , "mov r9, 1"
    , "cmp r9, r10"
    , "je true_" <> Text.pack (show addr)
    , "jmp false_" <> Text.pack (show addr)
    , "true_" <> Text.pack (show addr) <> ":"
    , "mov rsi, True"
    , "mov rdx, 5"
    , "jmp end_" <> Text.pack (show addr)
    , "false_" <> Text.pack (show addr) <> ":"
    , "mov rsi, False"
    , "mov rdx, 6"
    , "end_" <> Text.pack (show addr) <> ":"
    , "syscall"
    ]
emitInstr (Addr addr) op@(OpCodeIntr (Jet (Addr jmpAddr))) =
  joinLines
    [ ";; " <> Text.pack (show op)
    , "pop r10"
    , "mov r9, 1"
    , "cmp r9, r10"
    , "je true_" <> Text.pack (show addr)
    , "jmp end_" <> Text.pack (show addr)
    , "true_" <> Text.pack (show addr) <> ":"
    , "jmp ip_" <> Text.pack (show jmpAddr)
    , "end_" <> Text.pack (show addr) <> ":"
    ]
emitInstr _ op@(OpCodePushToCallStack (Addr currAddr) (Addr jmpAddr)) =
  joinLines
    [ ";; " <> Text.pack (show op)
    , "add r15, 8"
    , "mov qword [r15], ip_" <> Text.pack (show currAddr)
    , "jmp ip_" <> Text.pack (show jmpAddr)
    ]
emitInstr _ op@(OpCodePopJmpFromCallStack) =
  joinLines
    [ ";; " <> Text.pack (show op)
    , "mov r14, [r15]"
    , "sub r15, 8"
    , "jmp r14"
    ]

joinLines :: [Text] -> Text
joinLines = Text.intercalate "\n" . (<> ["\n"])

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Horth.Types where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import Text.Megaparsec.Pos (SourcePos)

-- * Bare VM

data OpCode
  = OpCodePushLit Lit
  | OpCodeIntr Intrinsic
  | OpCodePushToCallStack Addr Addr
  | OpCodePopJmpFromCallStack
  deriving stock (Eq, Show)

prettyOpCode :: OpCode -> Text
prettyOpCode (OpCodePushLit lit) = "push " <> prettyLit lit
prettyOpCode (OpCodeIntr intr) = prettyIntrinsic intr
prettyOpCode (OpCodePushToCallStack retAddr callAddr) = "pushCall " <> prettyAddr retAddr <> " " <> prettyAddr callAddr
prettyOpCode OpCodePopJmpFromCallStack = "popCall"

data Lit
  = LitInt Int64
  | LitBool Bool
  | LitStr ByteString
  | LitPtr Word64
  deriving stock (Show, Eq)

prettyLit :: Lit -> Text
prettyLit (LitInt i) = Text.pack $ show i
prettyLit (LitBool True) = "true"
prettyLit (LitBool False) = "false"
prettyLit (LitStr str) = Text.pack $ show str
prettyLit (LitPtr ptr) = "*" <> Text.pack (show ptr)

newtype Addr = Addr {getAddr :: Word64}
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num)

prettyAddr :: Addr -> Text
prettyAddr (Addr addr) = "#" <> Text.pack (show addr)

data Intrinsic
  = -- | Add the top two elements of the stack, pushing the result
    Add
  | -- | Subtract the top two elements of the stack, pushing the result
    Sub
  | -- | Multiply the top two elements of the stack, pushing the result
    Mul
  | -- | Divide the top two elements of the stack, pushing the result
    Div
  | -- | Compare the top two elements of the stack for equality, pushing the result
    EqI
  | -- | Negate the top element of the stack
    Not
  | -- | Jump to the address on the top of the stack
    Jmp Addr
  | -- | (addr : cond : _) -> if cond then jump to addr else continue
    Jet Addr
  | -- | Duplicate the top element of the stack
    Dup
  | -- | Swap the top two elements of the stack
    Swap
  | Rot
  | -- | Pop the top element of the stack
    Pop
  | -- | Duplicate the second element of the stack
    Over
  | -- | Read one byte from a ptr
    Read1
  | Read4
  | Write1
  | Mem
  | SysCall0
  | SysCall1
  | SysCall2
  | SysCall3
  | SysCall4
  | SysCall5
  | SysCall6
  | UnsafeMkPtr
  | Rename Text
  deriving stock (Show, Eq)

prettyIntrinsic :: Intrinsic -> Text
prettyIntrinsic intr = Text.toLower $ Text.pack $ show intr

newtype Stack = Stack {getStack :: [Lit]}
  deriving stock (Show, Eq)

data HType
  = HInt
  | HBool
  | HAddr
  | HPtr
  | HTypeVar Integer (Maybe [HType])
  deriving stock (Show, Eq)

newtype TypeStack = TypeStack {getTypeStack :: [(HType, Maybe Text)]}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

-- * Rich AST

data Ast
  = AstPushLit Lit SourcePos
  | AstIntr Intrinsic SourcePos
  | AstInclude FilePath SourcePos
  | AstName Text SourcePos
  | AstIf [Ast] [Ast] SourcePos
  | AstProc Bool Text TypeStack TypeStack [Ast] SourcePos
  | AstHole Text SourcePos
  deriving stock (Show, Eq)

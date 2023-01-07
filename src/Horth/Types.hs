{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Horth.Types where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Text.Megaparsec.Pos (SourcePos)

-- * Bare VM

data OpCode
  = OpCodePushLit Lit
  | OpCodeIntr Intrinsic
  | OpCodePushToCallStack Addr
  | OpCodePopJmpFromCallStack
  deriving stock (Eq, Show)

prettyOpCode :: OpCode -> Text
prettyOpCode (OpCodePushLit lit) = "push " <> prettyLit lit
prettyOpCode (OpCodeIntr intr) = prettyIntrinsic intr
prettyOpCode (OpCodePushToCallStack addr) = "pushCall " <> prettyAddr addr
prettyOpCode OpCodePopJmpFromCallStack = "popCall"

data Lit
  = LitInt Integer
  | LitBool Bool
  | LitAddr Addr
  deriving stock (Show, Eq)

prettyLit :: Lit -> Text
prettyLit (LitInt i) = Text.pack $ show i
prettyLit (LitBool True) = "true"
prettyLit (LitBool False) = "false"
prettyLit (LitAddr addr) = prettyAddr addr

newtype Addr = Addr {getAddr :: Int}
  deriving stock (Show, Eq)
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
    Jmp
  | -- | (addr : cond : _) -> if cond then jump to addr else continue
    Jet
  | -- | Duplicate the top element of the stack
    Dup
  | -- | Swap the top two elements of the stack
    Swap
  | -- | Pop the top element of the stack
    Pop
  | -- | Duplicate the second element of the stack
    Over
  deriving stock (Show, Eq)

prettyIntrinsic :: Intrinsic -> Text
prettyIntrinsic intr = Text.toLower $ Text.pack $ show intr

newtype Stack = Stack {getStack :: [Lit]}
  deriving stock (Show, Eq)

newtype Code = Code {getCode :: Vector OpCode}
  deriving stock (Show, Eq)

data HType
  = HInt
  | HBool
  | HAddr
  | HTypeVar Integer
  deriving stock (Show, Eq)

-- * Rich AST

data Ast
  = AstPushLit Lit SourcePos
  | AstIntr Intrinsic SourcePos
  | AstName Text SourcePos
  | AstIf [Ast] SourcePos
  | AstProc Text [HType] [HType] [Ast] SourcePos
  deriving stock (Show, Eq)

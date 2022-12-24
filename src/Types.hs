module Types where

import Data.Text (Text)
import Data.Kind (Type)
import Data.Void (Void)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Control.Monad.Reader (MonadReader, Reader)
import Control.Monad.State (MonadState, StateT)
import Data.Vector (Vector)

data OpCode' (f :: Type -> Type)
  = PushLit Lit
  -- ^ Push a literal value onto the stack
  | Intr Intrinsic
  -- ^ Perform an intrinsic operation
  | Name Text
  -- ^ Push a named value onto the stack
  | LabelDecl (f Text)
  -- ^ Declare a label, which can be jumped to. noop in compiled code
  | LabelRef (f Text)
  -- ^ Resolves to address of the label

deriving stock instance Show (f Text) => Show (OpCode' f)
deriving stock instance Eq (f Text) => Eq (OpCode' f)

type OpCode = OpCode' (Const Void)

type OpCodeWithLabels = OpCode' Identity

data Lit
  = LitInt Integer
  | LitBool Bool
  | LitAddr Addr
  deriving stock (Show, Eq)

newtype Addr = Addr {getAddr :: Int}
  deriving stock (Show, Eq)
  deriving newtype Num

data Intrinsic
  = Add
  -- ^ Add the top two elements of the stack, pushing the result
  | Sub
  -- ^ Subtract the top two elements of the stack, pushing the result
  | Mul
  -- ^ Multiply the top two elements of the stack, pushing the result
  | Div
  -- ^ Divide the top two elements of the stack, pushing the result
  | EqI
  -- ^ Compare the top two elements of the stack for equality, pushing the result
  | Not
  -- ^ Negate the top element of the stack
  | Jet
  -- ^ (addr : cond : _) -> if cond then jump to addr else continue
  | Dup
  -- ^ Duplicate the top element of the stack
  | Swap
  -- ^ Swap the top two elements of the stack
  | Pop
  | Over
  deriving stock (Show, Eq)

newtype Stack = Stack {getStack :: [Lit]}
  deriving stock (Show, Eq)

newtype Code = Code {getCode :: Vector OpCode}
  deriving stock (Show, Eq)

data MachineState = MachineState
  { stack :: Stack
  , pc :: Addr
  }
  deriving stock (Show, Eq)

newtype Machine a = Machine {runMachine :: StateT MachineState (Reader Code) a}
  deriving newtype (Functor, Applicative, Monad, MonadState MachineState, MonadReader Code)

instance MonadFail Machine where
  fail = error

data HType
  = HInt
  | HBool
  | HAddr
  | HTypeVar Integer
  deriving stock (Show, Eq)

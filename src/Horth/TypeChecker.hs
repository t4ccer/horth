module Horth.TypeChecker (typeCheck, TypeError, TypeCheckedAst (getTypeCheckedAst)) where

import Control.Monad (unless, void, when)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, ask, local, runReaderT)
import Control.Monad.State (MonadState, StateT, execStateT, gets, modify)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Megaparsec.Pos (SourcePos, sourcePosPretty)

import Horth.Types

type TypeError = Text

newtype TypeCheckedAst = TypeCheckedAst {getTypeCheckedAst :: [Ast]}
  deriving stock (Eq, Show)

data TypeCheckState = TypeCheckState
  { typeCheckStack :: [HType]
  , typeCheckLabels :: [(Text, ([HType], [HType]))]
  , typeCheckUsedTyVars :: Integer
  }

newtype TypeCheckMachine a = TypeCheckMachine
  { runTypeCheckMachine :: StateT TypeCheckState (ReaderT (NonEmpty Ast) (Either TypeError)) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadState TypeCheckState, MonadReader (NonEmpty Ast), MonadError TypeError)

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

stacksEqual ::
  forall (n :: Peano).
  Vec n HType ->
  Vec n HType ->
  Bool
stacksEqual = stacksEqual' mempty
  where
    stacksEqual' :: forall (m :: Peano). [(Integer, HType)] -> Vec m HType -> Vec m HType -> Bool
    stacksEqual' _ Nil Nil = True
    stacksEqual' tyVars (HTypeVar tyIdx tys' :> ts1) (t2 :> ts2) =
      case lookup tyIdx tyVars of
        Nothing ->
          case tys' of
            Nothing -> stacksEqual' ((tyIdx, t2) : tyVars) ts1 ts2
            Just tys -> (t2 `elem` tys) && stacksEqual' ((tyIdx, t2) : tyVars) ts1 ts2
        Just t1 -> t1 == t2 && stacksEqual' tyVars ts1 ts2
    stacksEqual' tyVars (t1 :> ts1) (t2 :> ts2) =
      t1 == t2 && stacksEqual' tyVars ts1 ts2

typeCheck :: [Ast] -> Either TypeError (TypeCheckedAst, [HType])
typeCheck [] = pure (TypeCheckedAst [], [])
typeCheck ast@(a : as) =
  fmap (\st -> (TypeCheckedAst ast, typeCheckStack st))
    . flip runReaderT (a :| as)
    . flip execStateT (TypeCheckState [] [] 0)
    . runTypeCheckMachine
    $ go
  where
    pushType :: HType -> TypeCheckMachine ()
    pushType (HTypeVar _ _) = error "pushType: Implementation bug - Type variable pushed"
    pushType t = modify (\s -> s {typeCheckStack = t : typeCheckStack s})

    popTypes ::
      forall (n :: Peano).
      Vec n HType ->
      SourcePos ->
      TypeCheckMachine (Vec n HType)
    popTypes t pos = do
      op :| _ <- ask
      let expectedLen = vecLength t
      stackLst <- gets ((take expectedLen) . typeCheckStack)
      let prettyOp = \case
            AstIntr intr _ -> prettyIntrinsic intr
            AstIf _ _ _ -> "if"
            _ -> error "It shouldn't be here"
      let err =
            throwError $
              Text.pack $
                mconcat
                  [ sourcePosPretty pos
                  , ": ERROR: "
                  , "Stack top mismatch when calling '"
                  , Text.unpack $ prettyOp op
                  , "'\n"
                  , "    Expected:\n"
                  , "        "
                  , show $ vecToList t
                  , "\n    Got:\n"
                  , "        "
                  , show stackLst
                  ]
      when (length stackLst /= expectedLen) err
      -- 'vecFromList' is safe here because we checked the length of the list
      let stack :: Vec n HType = vecFromList stackLst t
      unless (stacksEqual t stack) err
      modify (\s -> s {typeCheckStack = drop expectedLen (typeCheckStack s)})
      pure stack

    generateTyVar :: TypeCheckMachine HType
    generateTyVar = do
      n <- gets typeCheckUsedTyVars
      modify (\s -> s {typeCheckUsedTyVars = typeCheckUsedTyVars s + 1})
      pure $ HTypeVar n Nothing

    generateTyVar' :: [HType] -> TypeCheckMachine HType
    generateTyVar' tys = do
      n <- gets typeCheckUsedTyVars
      modify (\s -> s {typeCheckUsedTyVars = typeCheckUsedTyVars s + 1})
      pure $ HTypeVar n (Just tys)

    continueLinear :: [Ast] -> TypeCheckMachine ()
    continueLinear ast = case nonEmpty ast of
      Nothing -> pure ()
      Just ast -> local (const ast) go

    saveProcType :: Text -> [HType] -> [HType] -> TypeCheckMachine ()
    saveProcType name inType outType =
      modify
        ( \s ->
            s
              { typeCheckLabels = (name, (inType, outType)) : typeCheckLabels s
              }
        )

    getProcType :: Text -> SourcePos -> TypeCheckMachine ([HType], [HType])
    getProcType name pos = do
      labels <- gets typeCheckLabels
      case lookup name labels of
        Nothing ->
          throwError $
            Text.pack $
              mconcat
                [ sourcePosPretty pos
                , ": ERROR: "
                , "Unknown procedure call: '"
                , show name
                , "'"
                ]
        Just tys -> pure tys

    go :: TypeCheckMachine ()
    go = do
      currAst :| restAst <- ask

      case currAst of
        AstPushLit (LitInt _) _ -> do
          pushType HInt
          continueLinear restAst
        AstPushLit (LitBool _) _ -> do
          pushType HBool
          continueLinear restAst
        AstPushLit (LitStrPtr _ _) _ -> do
          pushType HStrPtr
          continueLinear restAst
        AstIntr Add pos -> do
          b <- generateTyVar' [HInt, HStrPtr]
          _ :> b' :> Nil <- popTypes (HInt :> b :> Nil) pos
          pushType b'
          continueLinear restAst
        AstIntr Sub pos -> do
          void $ popTypes (HInt :> HInt :> Nil) pos
          pushType HInt
          continueLinear restAst
        AstIntr Mul pos -> do
          void $ popTypes (HInt :> HInt :> Nil) pos
          pushType HInt
          continueLinear restAst
        AstIntr Div pos -> do
          void $ popTypes (HInt :> HInt :> Nil) pos
          pushType HInt
          continueLinear restAst
        AstIntr EqI pos -> do
          void $ popTypes (HInt :> HInt :> Nil) pos
          pushType HBool
          continueLinear restAst
        AstIntr Not pos -> do
          void $ popTypes (HBool :> Nil) pos
          pushType HBool
          continueLinear restAst
        AstIntr Swap pos -> do
          a <- generateTyVar
          b <- generateTyVar
          (a' :> b' :> Nil) <- popTypes (a :> b :> Nil) pos
          pushType a'
          pushType b'
          continueLinear restAst
        AstIntr Dup pos -> do
          a <- generateTyVar
          (a' :> Nil) <- popTypes (a :> Nil) pos
          pushType a'
          pushType a'
          continueLinear restAst
        AstIntr Pop pos -> do
          a <- generateTyVar
          void $ popTypes (a :> Nil) pos
          continueLinear restAst
        AstIntr Over pos -> do
          a <- generateTyVar
          b <- generateTyVar
          (a' :> b' :> Nil) <- popTypes (a :> b :> Nil) pos
          pushType b'
          pushType a'
          pushType b'
          continueLinear restAst
        AstIntr PrintI pos -> do
          void $ popTypes (HInt :> Nil) pos
          continueLinear restAst
        AstIntr PrintB pos -> do
          void $ popTypes (HBool :> Nil) pos
          continueLinear restAst
        AstIntr PrintS pos -> do
          void $ popTypes (HInt :> HStrPtr :> Nil) pos
          continueLinear restAst
        AstIntr Read1 pos -> do
          void $ popTypes (HStrPtr :> Nil) pos
          pushType HInt
          continueLinear restAst
        AstIntr (Jmp _) _ -> do
          error "'jmp' shouldn't be in the AST"
        AstIntr (Jet _) _ -> do
          error "'jet' shouldn't be in the AST"
        AstIf ifAst pos _ -> do
          void $ popTypes (HBool :> Nil) pos
          case nonEmpty ifAst of
            Nothing -> pure ()
            Just ifAst' -> do
              labels <- gets typeCheckLabels
              preIfStack <- gets typeCheckStack
              local (const ifAst') go
              postIfStack <- gets typeCheckStack
              unless (preIfStack == postIfStack) $
                throwError $
                  Text.pack $
                    mconcat
                      [ sourcePosPretty pos
                      , ": ERROR: "
                      , "'if' cannot change the type stack.\n"
                      , "    Before 'if' branch:\n"
                      , "        "
                      , show preIfStack
                      , "\n"
                      , "    After if branch:\n"
                      , "        "
                      , show postIfStack
                      ]
              modify (\s -> s {typeCheckLabels = labels})
          continueLinear restAst
        AstProc procName inStack outStack procAst pos _ -> do
          saveProcType procName inStack outStack
          preProcStack <- gets typeCheckStack

          modify (\s -> s {typeCheckStack = inStack})

          case nonEmpty procAst of
            Nothing -> pure ()
            Just procAst' -> do
              labels <- gets typeCheckLabels
              local (const procAst') go
              modify (\s -> s {typeCheckLabels = labels})
          postProcStack <- gets typeCheckStack
          unless (postProcStack == outStack) $
            throwError $
              Text.pack $
                mconcat
                  [ sourcePosPretty pos
                  , ": ERROR: proc '"
                  , Text.unpack procName
                  , "' type missmatch.\n"
                  , "    Procedure declared output type to be:\n"
                  , "        "
                  , show outStack
                  , "\n"
                  , "    But is:\n"
                  , "        "
                  , show postProcStack
                  ]

          modify (\s -> s {typeCheckStack = preProcStack})
          continueLinear restAst
        AstName name pos -> do
          (inType, outType) <- getProcType name pos
          stackTop <- gets ((take (length inType)) . typeCheckStack)
          unless (stackTop == inType) $
            throwError $
              Text.pack $
                mconcat
                  [ sourcePosPretty pos
                  , ": ERROR: proc '"
                  , Text.unpack name
                  , "' call type missmatch.\n"
                  , "    To call the procedure, stack top is expected: "
                  , show inType
                  , ", got: "
                  , show stackTop
                  ]
          modify (\s -> s {typeCheckStack = drop (length inType) (typeCheckStack s)})
          modify (\s -> s {typeCheckStack = outType ++ typeCheckStack s})
          continueLinear restAst
        AstHole holeName pos -> do
          currStack <- gets typeCheckStack
          throwError $
            Text.pack $
              mconcat
                [ sourcePosPretty pos
                , ": ERROR: hole '"
                , Text.unpack holeName
                , "'\n"
                , "    Stack has type:\n"
                , "        "
                , show currStack
                ]

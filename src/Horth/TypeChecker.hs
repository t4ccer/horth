module Horth.TypeChecker (typeCheck, TypeError, TypeCheckedAst (getTypeCheckedAst)) where

import Control.Monad (unless, void, when)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, ask, local, runReaderT)
import Control.Monad.State (MonadState, StateT, execStateT, gets, modify)
import Data.Kind (Type)
import Data.List (intercalate, nub)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Megaparsec.Pos (SourcePos, sourcePosPretty)

import Horth.Types

type TypeError = Text

newtype TypeCheckedAst = TypeCheckedAst {getTypeCheckedAst :: [Ast]}
  deriving stock (Eq, Show)

data TypeCheckState = TypeCheckState
  { typeCheckStack :: TypeStack
  , typeCheckLabels :: [(Text, (TypeStack, TypeStack))]
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
deriving stock instance Functor (Vec n)

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

typeCheck :: [Ast] -> Either TypeError (TypeCheckedAst, TypeStack)
typeCheck [] = pure (TypeCheckedAst [], TypeStack [])
typeCheck ast@(a : as) = do
  let initStack =
        TypeStack
          [ (HInt, Just "argc")
          , (HPtr, Just "argv")
          ]
  fmap (\st -> (TypeCheckedAst ast, typeCheckStack st))
    . flip runReaderT (a :| as)
    . flip execStateT (TypeCheckState initStack [] 0)
    . runTypeCheckMachine
    $ go
  where
    pushType' :: (HType, Maybe Text) -> TypeCheckMachine ()
    pushType' (HTypeVar _ _, _) = error "pushType: Implementation bug - Type variable pushed"
    pushType' t = modify (\s -> s {typeCheckStack = TypeStack (t : getTypeStack (typeCheckStack s))})

    pushType :: HType -> TypeCheckMachine ()
    pushType t = pushType' (t, Nothing)

    pushTypeNamed :: HType -> Text -> TypeCheckMachine ()
    pushTypeNamed t name = pushType' (t, Just name)

    popTypes ::
      forall (n :: Peano).
      Vec n HType ->
      SourcePos ->
      TypeCheckMachine (Vec n (HType, Maybe Text))
    popTypes t pos = do
      op :| _ <- ask
      let expectedLen = vecLength t
      stackLst <- gets ((take expectedLen) . getTypeStack . typeCheckStack)
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
                  , prettyStack $ TypeStack $ fmap (,Nothing) $ vecToList t
                  , "\n    Got:\n"
                  , "        "
                  , prettyStack $ TypeStack stackLst
                  ]
      when (length stackLst /= expectedLen) err
      -- 'vecFromList' is safe here because we checked the length of the list
      let stack :: Vec n (HType, Maybe Text) = vecFromList stackLst t
      unless (stacksEqual t $ fmap fst stack) err
      modify (\s -> s {typeCheckStack = TypeStack $ drop expectedLen $ getTypeStack $ typeCheckStack s})
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

    saveProcType :: Text -> TypeStack -> TypeStack -> TypeCheckMachine ()
    saveProcType name inType outType =
      modify
        ( \s ->
            s
              { typeCheckLabels = (name, (inType, outType)) : typeCheckLabels s
              }
        )

    getProcType :: Text -> SourcePos -> TypeCheckMachine (TypeStack, TypeStack)
    getProcType name pos = do
      labels <- gets typeCheckLabels
      case lookup name labels of
        Nothing ->
          throwError $
            Text.pack $
              mconcat
                [ sourcePosPretty pos
                , ": ERROR: "
                , "Unknown procedure call: "
                , Text.unpack name
                ]
        Just tys -> pure tys

    go :: TypeCheckMachine ()
    go = do
      currAst :| restAst <- ask

      case currAst of
        AstPushLit (LitInt lit) _ -> do
          pushTypeNamed HInt $ Text.pack $ show lit
          continueLinear restAst
        AstPushLit (LitBool _) _ -> do
          pushType HBool
          continueLinear restAst
        AstPushLit (LitStr _) _ -> do
          pushType HPtr
          continueLinear restAst
        AstPushLit (LitPtr _) _ -> do
          pushType HPtr
          continueLinear restAst
        AstIntr Add pos -> do
          b <- generateTyVar' [HInt, HPtr]
          a' :> b' :> Nil <- popTypes (HInt :> b :> Nil) pos
          let newName = case (snd a', snd b') of
                (Just aName, Just bName) -> Just $ bName <> "+" <> aName
                _ -> Nothing
          pushType' $ changeName newName b'
          continueLinear restAst
        AstIntr Sub pos -> do
          b <- generateTyVar' [HInt, HPtr]
          a' :> b' :> Nil <- popTypes (HInt :> b :> Nil) pos
          let newName = case (snd a', snd b') of
                (Just aName, Just bName) -> Just $ bName <> "-" <> aName
                _ -> Nothing
          pushType' $ changeName newName b'
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
          pushType' a'
          pushType' b'
          continueLinear restAst
        AstIntr Dup pos -> do
          a <- generateTyVar
          (a' :> Nil) <- popTypes (a :> Nil) pos
          pushType' a'
          pushType' a'
          continueLinear restAst
        AstIntr Pop pos -> do
          a <- generateTyVar
          void $ popTypes (a :> Nil) pos
          continueLinear restAst
        AstIntr Over pos -> do
          a <- generateTyVar
          b <- generateTyVar
          (a' :> b' :> Nil) <- popTypes (a :> b :> Nil) pos
          pushType' b'
          pushType' a'
          pushType' b'
          continueLinear restAst
        AstIntr Rot pos -> do
          a <- generateTyVar
          b <- generateTyVar
          c <- generateTyVar
          (a' :> b' :> c' :> Nil) <- popTypes (a :> b :> c :> Nil) pos
          pushType' a'
          pushType' c'
          pushType' b'
          continueLinear restAst
        AstIntr Read1 pos -> do
          void $ popTypes (HPtr :> Nil) pos
          pushType HInt
          continueLinear restAst
        AstIntr Read4 pos -> do
          void $ popTypes (HPtr :> Nil) pos
          pushType HInt
          continueLinear restAst
        AstIntr Write1 pos -> do
          void $ popTypes (HPtr :> HInt :> Nil) pos
          continueLinear restAst
        AstIntr Mem _ -> do
          pushTypeNamed HPtr "#mem#"
          continueLinear restAst
        AstIntr SysCall0 pos -> do
          void $ popTypes (HInt :> Nil) pos
          pushType HInt
          continueLinear restAst
        AstIntr SysCall1 pos -> do
          rdi <- generateTyVar' [HInt, HPtr]
          void $ popTypes (HInt :> rdi :> Nil) pos
          pushType HInt
          continueLinear restAst
        AstIntr SysCall2 pos -> do
          rdi <- generateTyVar' [HInt, HPtr]
          rsi <- generateTyVar' [HInt, HPtr]
          void $ popTypes (HInt :> rdi :> rsi :> Nil) pos
          pushType HInt
          continueLinear restAst
        AstIntr SysCall3 pos -> do
          rdi <- generateTyVar' [HInt, HPtr]
          rsi <- generateTyVar' [HInt, HPtr]
          rdx <- generateTyVar' [HInt, HPtr]
          void $ popTypes (HInt :> rdi :> rsi :> rdx :> Nil) pos
          pushType HInt
          continueLinear restAst
        AstIntr SysCall4 pos -> do
          rdi <- generateTyVar' [HInt, HPtr]
          rsi <- generateTyVar' [HInt, HPtr]
          rdx <- generateTyVar' [HInt, HPtr]
          r10 <- generateTyVar' [HInt, HPtr]
          void $ popTypes (HInt :> rdi :> rsi :> rdx :> r10 :> Nil) pos
          pushType HInt
          continueLinear restAst
        AstIntr SysCall5 pos -> do
          rdi <- generateTyVar' [HInt, HPtr]
          rsi <- generateTyVar' [HInt, HPtr]
          rdx <- generateTyVar' [HInt, HPtr]
          r10 <- generateTyVar' [HInt, HPtr]
          r8 <- generateTyVar' [HInt, HPtr]
          void $ popTypes (HInt :> rdi :> rsi :> rdx :> r10 :> r8 :> Nil) pos
          pushType HInt
          continueLinear restAst
        AstIntr SysCall6 pos -> do
          rdi <- generateTyVar' [HInt, HPtr]
          rsi <- generateTyVar' [HInt, HPtr]
          rdx <- generateTyVar' [HInt, HPtr]
          r10 <- generateTyVar' [HInt, HPtr]
          r8 <- generateTyVar' [HInt, HPtr]
          r9 <- generateTyVar' [HInt, HPtr]
          void $ popTypes (HInt :> rdi :> rsi :> rdx :> r10 :> r8 :> r9 :> Nil) pos
          pushType HInt
          continueLinear restAst
        AstIntr UnsafeMkPtr pos -> do
          void $ popTypes (HInt :> Nil) pos
          pushType HPtr
          continueLinear restAst
        AstIntr (Rename newName) pos -> do
          a <- generateTyVar
          (a' :> Nil) <- popTypes (a :> Nil) pos
          pushType' $ changeName (Just newName) a'
          continueLinear restAst
        AstIntr (Jmp _) _ -> do
          error "'jmp' shouldn't be in the AST"
        AstIntr (Jet _) _ -> do
          error "'jet' shouldn't be in the AST"
        AstInclude _ _ -> do
          error "'include's should be already resolved"
        AstIf ifAst elseAst pos -> do
          void $ popTypes (HBool :> Nil) pos
          stack <- gets typeCheckStack
          labels <- gets typeCheckLabels
          ifStack <- case nonEmpty ifAst of
            Nothing -> pure stack
            Just ifAst' -> do
              local (const ifAst') go
              postIfStack <- gets typeCheckStack
              modify (\s -> s {typeCheckLabels = labels, typeCheckStack = stack})
              pure postIfStack

          elseStack <- case nonEmpty elseAst of
            Nothing -> pure stack
            Just elseAst' -> do
              local (const elseAst') go
              postElseStack <- gets typeCheckStack
              modify (\s -> s {typeCheckLabels = labels, typeCheckStack = stack})
              pure postElseStack

          unless (fmap fst (getTypeStack ifStack) == fmap fst (getTypeStack elseStack)) $
            throwError $
              Text.pack $
                mconcat
                  [ sourcePosPretty pos
                  , ": ERROR: "
                  , "Both 'if' branches must have the same type.\n"
                  , "    True 'if' branch:\n"
                  , "        "
                  , prettyStack ifStack
                  , "\n"
                  , "    False if branch:\n"
                  , "        "
                  , prettyStack elseStack
                  ]
          modify (\s -> s {typeCheckStack = elseStack})
          continueLinear restAst
        AstProc isInline procName inStack outStack procAst pos -> do
          unless isInline $
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
          unless (fmap fst (getTypeStack postProcStack) == fmap fst (getTypeStack outStack)) $
            throwError $
              Text.pack $
                mconcat
                  [ sourcePosPretty pos
                  , ": ERROR: proc '"
                  , Text.unpack procName
                  , "' type missmatch.\n"
                  , "    Procedure declared output type to be:\n"
                  , "        "
                  , prettyStack $ outStack
                  , "\n"
                  , "    But is:\n"
                  , "        "
                  , prettyStack postProcStack
                  ]

          modify (\s -> s {typeCheckStack = preProcStack})
          when isInline $
            saveProcType procName inStack outStack
          continueLinear restAst
        AstName name pos -> do
          (inType, outType) <- getProcType name pos
          stackTop <- gets ((take (length $ getTypeStack inType)) . getTypeStack . typeCheckStack)
          -- FIXME: Supoprt type variables in 'inType'
          unless (fmap fst stackTop == fmap fst (getTypeStack inType)) $
            throwError $
              Text.pack $
                mconcat
                  [ sourcePosPretty pos
                  , ": ERROR: proc '"
                  , Text.unpack name
                  , "' call type missmatch.\n"
                  , "    To call the procedure, stack top is expected: "
                  , prettyStack inType
                  , ", got: "
                  , prettyStack $ TypeStack stackTop
                  ]
          modify (\s -> s {typeCheckStack = TypeStack (drop (length $ getTypeStack inType) (getTypeStack $ typeCheckStack s))})
          modify (\s -> s {typeCheckStack = outType <> typeCheckStack s})
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
                , prettyStack currStack
                ]

changeName :: Maybe Text -> (HType, Maybe Text) -> (HType, Maybe Text)
changeName name (t, _) = (t, name)

prettyStack :: TypeStack -> String
prettyStack (TypeStack []) = "<empty>"
prettyStack (TypeStack stack) = unwords $ map prettyTy stack
  where
    matchTyVar :: (HType, Maybe Text) -> Maybe (Integer, (Maybe [HType]))
    matchTyVar (HTypeVar idx constraints, _name) = Just (idx, constraints)
    matchTyVar _ = Nothing

    tyVarMappings :: [(Integer, String)]
    tyVarMappings =
      fmap (\(idx, constraints) -> (idx, prettyTyVar idx constraints))
        . mapMaybe matchTyVar
        $ stack

    usedIndices :: [Integer]
    usedIndices = nub $ map fst tyVarMappings

    prettyIndex :: Integer -> String
    prettyIndex idx = (: []) $ fromMaybe undefined $ lookup idx $ zip usedIndices ['a' ..]

    prettyTyVar :: Integer -> Maybe [HType] -> String
    prettyTyVar idx Nothing = prettyIndex idx
    prettyTyVar idx (Just constraints) = "(" <> prettyIndex idx <> " := " <> prettyConstraints constraints <> ")"

    prettyConstraints :: [HType] -> String
    prettyConstraints constraints = intercalate " | " $ map (prettyTy . (,Nothing)) constraints

    prettyTy :: (HType, Maybe Text) -> String
    prettyTy (HInt, name) = prettyName name "int"
    prettyTy (HBool, name) = prettyName name "bool"
    prettyTy (HAddr, name) = prettyName name "#addr#" -- NOTE: this should never be printed
    prettyTy (HPtr, name) = prettyName name "ptr"
    prettyTy (HTypeVar idx _, _name) = fromMaybe undefined $ lookup idx tyVarMappings

    prettyName :: Maybe Text -> String -> String
    prettyName Nothing t = t
    prettyName (Just name) t = "(" <> Text.unpack name <> " := " <> t <> ")"

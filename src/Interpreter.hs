{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interpreter where

import Utils
import PrettyPrinter
import Control.Monad.State
import Control.Monad.Except

-- | type for c++ variable
data Var = Var {varName :: Name, varValue :: CConst} deriving Show

-- | type for evaluated c++ function
data StateFunc = StateFunc {sFuncType :: CType, sFuncName :: Name, sFuncArgs :: [ArgVar], sFuncBody :: AnyFuncM}

-- | type for c++ program state
data ProgramState = ProgramState {
  vars :: [Var]          -- ^ defined variables
  , funcs :: [StateFunc] -- ^ defined functions
  }

-- | type for c++ evaluated code
type FuncM a = StateT ProgramState (ExceptT InterpretError IO) a

-- | type for c++ code that returns void
type VoidFuncM = FuncM ()

-- | type for c++ code that returns string
type StringFuncM = FuncM String

-- | type for c++ code that returns int
type IntFuncM = FuncM Int

-- | type for c++ code that returns double
type DoubleFuncM = FuncM Double

-- | type for c++ code that returns bool
type BoolFuncM = FuncM Bool

-- | type for c++ code that returns any value
type AnyFuncM = FuncM CConst

-- | type for interpretation errors
data InterpretError where
  -- | cant find variable
  NoVariable
    :: Name   -- ^ variable name
    -> String -- ^ extra information
    -> InterpretError

  -- | cant find function
  NoFunc
    :: Name   -- ^ function name
    -> String -- ^ extra information
    -> InterpretError

  -- | cant find main function
  NoMainFunc
    :: String -- ^ extra information
    -> InterpretError

  -- | error while performing io operation
  IOError
    :: String -- ^ extra information
    -> InterpretError

  -- | operation with wrong arguments
  BadOperation
    :: String -- ^ extra information
    -> InterpretError

  -- | type missmatch
  TypeMismatch
    :: CType  -- ^ expected type
    -> CType  -- ^ got type
    -> String -- ^ extra information
    -> InterpretError

  -- | more or less arguments in function call
  BadNumberOfArgs
    :: Name   -- ^ function name
    -> Int    -- ^ expected number of args
    -> Int    -- ^ got number of args
    -> String -- ^ extra information
    -> InterpretError

instance PrettyPrinter InterpretError where
  showPretty (NoVariable n extra) = "Cant find variable " ++ show n ++ "\n" ++ extra
  showPretty (NoFunc n extra) = "Cant find function named " ++ show n ++ "\n" ++ extra
  showPretty (NoMainFunc extra) = "Cant find main functon\n" ++ extra
  showPretty (IOError extra) = "IO error\n" ++ extra
  showPretty (TypeMismatch a b extra) = "Type mismatch: expected " ++ showPretty a ++ " got " ++ showPretty b ++ "\n" ++ extra
  showPretty (BadOperation extra) = "Bad operation\n" ++ extra
  showPretty (BadNumberOfArgs n a b extra) = "Bad number of arguments in " ++ show n ++ " call expected " ++
                                             show a ++ " was " ++ show b ++ "\n" ++ extra


-- | set new value to variable
setValue :: String -> Expr -> AnyFuncM
setValue targetName e = do
  st <- get
  findVar (vars st) [] where
    findVar [] _ = throwError $ NoVariable targetName ""
    findVar (syn@(Var curName val) : vs) beg 
      | targetName == curName = do
        newVal <- setVar val
        st <- get
        put $ ProgramState{vars = beg ++ Var curName newVal : vs, funcs = funcs st}
        return newVal
      | otherwise             = findVar vs (syn : beg)
    setVar (CIntVal _) = do
      newI <- evalIntExp e
      return $ CIntVal newI
    setVar (CDoubleVal _) = do
      newD <- evalDoubleExp e
      return $ CDoubleVal newD
    setVar (CStringVal _) = do
      newS <- evalStringExp e
      return $ CStringVal newS
    setVar (CBoolVal _) = do
      newB <- evalBoolExp e
      return $ CBoolVal newB

-- | add new variable
addVar :: Var -> ProgramState -> ProgramState
addVar var ps = ProgramState { vars = var : vars ps, funcs = funcs ps }

-- | get variable's value
getValue :: Name -> AnyFuncM
getValue targetName = do
  t <- get
  helper $ vars t
  where
    helper [] = throwError $ NoVariable targetName ""
    helper (Var curName res : tl)
      | curName == targetName = return res
      | otherwise             = helper tl

-- | get type of variable
getVarType :: Name -> FuncM CType
getVarType targetName = do
  t <- get
  helper $ vars t
  where
    helper [] = throwError $ NoVariable targetName ""
    helper (Var curName val : tl)
      | curName == targetName = return $ getConstType val
      | otherwise             = helper tl

-- | get type of CConst
getConstType :: CConst -> CType
getConstType (CStringVal _) = CString
getConstType (CIntVal _) = CInt
getConstType (CDoubleVal _) = CDouble
getConstType (CBoolVal _) = CBool

-- | map argument names with values
makeArg :: Name -> ArgVar -> CConst -> Except InterpretError Var
makeArg _ (ArgVar CString name) syn@(CStringVal _) = return $ Var name syn
makeArg _ (ArgVar CInt name) syn@(CIntVal _) = return $ Var name syn
makeArg _ (ArgVar CDouble name) syn@(CDoubleVal _) = return $ Var name syn
makeArg _ (ArgVar CBool name) syn@(CBoolVal _) = return $ Var name syn
makeArg funN (ArgVar tp varN) val = throwError $ TypeMismatch tp (getConstType val) ("  variable " ++ show varN ++ " in " ++ show funN ++ " call")

-- | extract CConst from each element
extractM :: [AnyFuncM] -> FuncM [CConst]
extractM [] = return []
extractM (e : es) = do
  e' <- e
  es' <- extractM es
  return (e' : es')

-- | evaluate any expression
evalAnyExp :: Expr -> AnyFuncM
evalAnyExp (ExpName n) = getValue n
evalAnyExp (ExpConstString s) = return $ CStringVal s
evalAnyExp (ExpConstInt i) = return $ CIntVal i
evalAnyExp (ExpConstDouble d) = return $ CDoubleVal d
evalAnyExp (ExpConstBool b) = return $ CBoolVal b
evalAnyExp (ExpFunCall name args) = do
  args' <- extractM $ fmap evalAnyExp args
  (ProgramState _ fs) <- get
  fun <- findFunc fs
  tmp <- callFunc fun args'
  res <- liftIO $ runExceptT $ evalStateT (sFuncBody fun) (ProgramState tmp fs)
  case res of
    syn@(Left _) -> liftEither syn
    (Right ok)   -> return ok
  where
    findFunc [] = throwError $ NoFunc name ""
    findFunc (f : fs)
      | sFuncName f == name = return f
      | otherwise           = findFunc fs
    callFunc fun args'
      | length (sFuncArgs fun) == length args = tt $ fmap (uncurry (makeArg name)) (zip (sFuncArgs fun) args')
      | otherwise                             = throwError $
                                                  BadNumberOfArgs (sFuncName fun) (length (sFuncArgs fun)) (length args) ""
    tt :: [Except InterpretError Var] -> FuncM [Var]
    tt [] = return []
    tt (e : es) = do
      case runExcept e of
        (Left err) -> throwError err
        (Right ok) -> do
          es' <- tt es
          return (ok : es')
evalAnyExp (ExpCin vs) = cinHelper vs where
  cinHelper [] = return $ CBoolVal True
  cinHelper (n : tl) = do
    vType <- getVarType n
    _ <- case vType of
      CString -> do
        res <- liftIO getLine
        setValue n (ExpConstString res)
      CInt -> do
        res <- liftIO getLine
        setValue n (ExpConstInt $ read res)
      CDouble -> do
        res <- liftIO getLine
        setValue n (ExpConstDouble $ read res)
      CBool -> do
        res <- liftIO getLine
        setValue n (ExpConstBool $ read res)
    cinHelper tl
evalAnyExp (ExpCout es) = coutHelper es where
  coutHelper [] = return $ CBoolVal True
  coutHelper (e : tl) = do
    e' <- evalAnyExp e
    liftIO $ putStr $ showPretty e'
    coutHelper tl
evalAnyExp (ExpAssign n e) = setValue n e
evalAnyExp (a :+: b) = do
  a' <- evalAnyExp a
  b' <- evalAnyExp b
  a' .+. b'
evalAnyExp (a :-: b) = do
  a' <- evalAnyExp a
  b' <- evalAnyExp b
  a' .-. b'
evalAnyExp (a :*: b) = do
  a' <- evalAnyExp a
  b' <- evalAnyExp b
  a' .*. b'
evalAnyExp (a :/: b) = do
  a' <- evalAnyExp a
  b' <- evalAnyExp b
  a' ./. b'
evalAnyExp (a :<: b) = do
  a' <- evalAnyExp a
  b' <- evalAnyExp b
  a' .<. b'
evalAnyExp (a :>: b) = do
  a' <- evalAnyExp a
  b' <- evalAnyExp b
  a' .>. b'
evalAnyExp (a :!=: b) = do
  a' <- evalAnyExp a
  b' <- evalAnyExp b
  a' .!=. b'
evalAnyExp (a :==: b) = do
  a' <- evalAnyExp a
  b' <- evalAnyExp b
  a' .==. b'


-- | evaluate string expression
evalStringExp :: Expr -> StringFuncM
evalStringExp e = do
  res <- evalAnyExp e
  case res of
      CStringVal s -> return s
      _ -> throwError $ TypeMismatch CString (getConstType res) ("  in expression " ++ showPretty e)

-- | evaluate int expression
evalIntExp :: Expr -> IntFuncM
evalIntExp e = do
  res <- evalAnyExp e
  case res of
      CIntVal i -> return i
      _ -> throwError $ TypeMismatch CInt (getConstType res) ("  in expression " ++ showPretty e)

-- | evaluate double expression
evalDoubleExp :: Expr -> DoubleFuncM
evalDoubleExp e = do
  res <- evalAnyExp e
  case res of
      CDoubleVal d -> return d
      _ -> throwError $ TypeMismatch CDouble (getConstType res) ("  in expression " ++ showPretty e)

-- | evaluate bool expression
evalBoolExp :: Expr -> BoolFuncM
evalBoolExp e = do
  res <- evalAnyExp e
  case res of
    CBoolVal b -> return b
    _ -> throwError $ TypeMismatch CBool (getConstType res) ("  in expression " ++ showPretty e)

-- | evaluate void expression
evalVoidExp :: Expr -> VoidFuncM
evalVoidExp e = do
  _ <- evalAnyExp e
  return ()

-- | add new string variable
addStringVar :: Name -> Expr -> VoidFuncM
addStringVar n e = do
  res <- evalStringExp e
  modify (addVar (Var n (CStringVal res)))

-- | add new int variable
addIntVar :: Name -> Expr -> VoidFuncM
addIntVar n e = do
  res <- evalIntExp e
  modify (addVar (Var n (CIntVal res)))

-- | add new double variable
addDoubleVar :: Name -> Expr -> VoidFuncM
addDoubleVar n e = do
  res <- evalDoubleExp e
  modify (addVar (Var n (CDoubleVal res)))

-- | add new bool variable
addBoolVar :: Name -> Expr -> VoidFuncM
addBoolVar n e = do
  res <- evalBoolExp e
  modify (addVar (Var n (CBoolVal res)))

-- | evaluate any function
makeAnyFunc :: Func -> AnyFuncM
makeAnyFunc (Func _ _ _ body) = helper body where
  helper [] = return $ CIntVal 0
  helper b = do
    b' <- execBody b
    case b' of
      Nothing -> return $ CIntVal 0
      (Just res) -> return res

  execBody :: Body -> FuncM (Maybe CConst)
  execBody [] = return Nothing
  execBody (Return e : _) = do
    e' <- evalAnyExp e
    return $ Just e'
  execBody (Val e : tl) = do
    evalVoidExp e
    execBody tl
  execBody (DefString n e : tl) = do
    addStringVar n e
    execBody tl
  execBody (DefInt n e : tl) = do
    addIntVar n e
    execBody tl
  execBody (DefDouble n e : tl) = do
    addDoubleVar n e
    execBody tl
  execBody (DefBool n e : tl) = do
    addBoolVar n e
    execBody tl
  execBody (If e thn Nothing : tl) = do
    cnd <- evalBoolExp e
    res <- if cnd then execBody thn
                  else return Nothing
    case res of
      Nothing -> execBody tl
      syn@(Just _) -> return syn
  execBody (If e thn (Just els) : tl) = do
    cnd <- evalBoolExp e
    res <- if cnd then execBody thn
                  else execBody els
    case res of
      Nothing -> execBody tl
      syn@(Just _) -> return syn
  execBody while@(While cnd whileBody : tl) = do
    res <- evalBoolExp cnd
    res' <- if res then do
      execBody whileBody
    else
      return Nothing
    case res' of
      Nothing -> if res then execBody while else execBody tl
      syn@(Just _) -> return syn

-- | evaluate string function
makeStringFunc :: Func -> StringFuncM
makeStringFunc syn@(Func CString _ _ _) = do
  res <- makeAnyFunc syn
  case res of
    CStringVal s -> return s
    _            -> throwError $ TypeMismatch CString (getConstType res) ("  in function " ++ funcName syn)
makeStringFunc f = throwError $ TypeMismatch CString (funcType f) ("  in function " ++ funcName f)

-- | evaluate int function
makeIntFuncM :: Func -> IntFuncM
makeIntFuncM syn@(Func CInt _ _ body) = helper body where
  helper [] = return 0
  helper _ = do
    res <- makeAnyFunc syn
    case res of
      CIntVal i -> return i
      _         -> throwError $ TypeMismatch CInt (getConstType res) ("  in function " ++ funcName syn)
makeIntFuncM f = throwError $ TypeMismatch CInt (funcType f) ("  in function " ++ funcName f)

-- | evaluate double function
makeDoubleFunc :: Func -> DoubleFuncM
makeDoubleFunc syn@(Func CDouble _ _ _) = do
  res <- makeAnyFunc syn
  case res of
    CDoubleVal d -> return d
    _         -> throwError $ TypeMismatch CDouble (getConstType res) ("  in function " ++ funcName syn)
makeDoubleFunc f = throwError $ TypeMismatch CDouble (funcType f) ("  in function " ++ funcName f)

-- | evaluate bool function
makeBoolFunc :: Func -> BoolFuncM
makeBoolFunc syn@(Func CBool _ _ _) = do
  res <- makeAnyFunc syn
  case res of
    CBoolVal b -> return b
    _         -> throwError $ TypeMismatch CBool (getConstType res) ("  in function " ++ funcName syn)
makeBoolFunc f = throwError $ TypeMismatch CInt (funcType f) ("  in function " ++ funcName f)

-- | find main function
getMain :: [Func] -> IntFuncM
getMain [] = throwError $ NoMainFunc ""
getMain (f : fs)
  | funcName f == "main" = if funcType f == CInt then makeIntFuncM f else throwError $ TypeMismatch CInt (funcType f) "  main type should be int"
  | otherwise            = getMain fs

-- | calculate sum
(.+.) :: CConst -> CConst -> AnyFuncM
(.+.) (CStringVal a) (CStringVal b) = return $ CStringVal (a ++ b)
(.+.) (CIntVal a) (CIntVal b) = return $ CIntVal (a + b)
(.+.) (CDoubleVal a) (CDoubleVal b) = return $ CDoubleVal (a + b)
(.+.) (CBoolVal True) (CBoolVal _) = return $ CBoolVal True
(.+.) (CBoolVal _) (CBoolVal True) = return $ CBoolVal True
(.+.) (CBoolVal _) (CBoolVal _) = return $ CBoolVal False
(.+.) _ _ = throwError $ BadOperation "+"

-- | calculate product
(.*.) :: CConst -> CConst -> AnyFuncM
(.*.) (CIntVal a) (CIntVal b) = return $ CIntVal (a * b)
(.*.) (CDoubleVal a) (CDoubleVal b) = return $ CDoubleVal (a * b)
(.*.) (CBoolVal True) (CBoolVal True) = return $ CBoolVal True
(.*.) (CBoolVal _) (CBoolVal _) = return $ CBoolVal False
(.*.) _ _ = throwError $ BadOperation "*"

-- | calculate differens
(.-.) :: CConst -> CConst -> AnyFuncM
(.-.) (CIntVal a) (CIntVal b) = return $ CIntVal (a - b)
(.-.) (CDoubleVal a) (CDoubleVal b) = return $ CDoubleVal (a - b)
(.-.) (CBoolVal True) (CBoolVal True) =return $  CBoolVal False
(.-.) (CBoolVal False) (CBoolVal False) = return $ CBoolVal False
(.-.) (CBoolVal _) (CBoolVal _) = return $ CBoolVal True
(.-.) _ _ = throwError $ BadOperation "-"

-- | calculate quotient
(./.) :: CConst -> CConst -> AnyFuncM
(./.) (CIntVal a) (CIntVal b) = return $ CIntVal (a `div` b)
(./.) (CDoubleVal a) (CDoubleVal b) = return $ CDoubleVal (a / b)
(./.) _ _ = throwError $ BadOperation "/"

-- | is left value less
(.<.) :: CConst -> CConst -> AnyFuncM
(.<.) (CIntVal a) (CIntVal b) = return $ CBoolVal $ a < b
(.<.) (CDoubleVal a) (CDoubleVal b) = return $ CBoolVal $ a < b
(.<.) _ _ = throwError $ BadOperation "<"

-- | is left value greater
(.>.) :: CConst -> CConst -> AnyFuncM
(.>.) (CIntVal a) (CIntVal b) = return $ CBoolVal $ a > b
(.>.) (CDoubleVal a) (CDoubleVal b) = return $ CBoolVal $ a > b
(.>.) _ _ = throwError $ BadOperation ">"

-- | is two values equal
(.==.) :: CConst -> CConst -> AnyFuncM
(.==.) (CIntVal a) (CIntVal b) = return $ CBoolVal $ a == b
(.==.) (CStringVal a) (CStringVal b) = return $ CBoolVal $ a == b
(.==.) (CDoubleVal a) (CDoubleVal b) = return $ CBoolVal $ a == b
(.==.) (CBoolVal a) (CBoolVal b) = return $ CBoolVal $ a == b
(.==.) _ _ = throwError $ BadOperation "=="

-- | is two values not equal
(.!=.) :: CConst -> CConst -> AnyFuncM
(.!=.) a b = do
  (CBoolVal res) <- a .==. b
  return $ CBoolVal $ not res
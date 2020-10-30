{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interpreter where

import Utils
import PrettyPrinter
import Control.Monad.State
import Control.Monad.Except

data Var = Var {varName :: Name, varValue :: CConst} deriving Show
data StateFunc = StateFunc {sFuncType :: CType, sFuncName :: Name, sFuncArgs :: [ArgVar], sFuncBody :: AnyFuncM}
data ProgramState = ProgramState {vars :: [Var], funcs :: [StateFunc]}

type FuncM a = StateT ProgramState (ExceptT InterpretError IO) a

type VoidFuncM = FuncM ()
type StringFuncM = FuncM String
type IntFuncM = FuncM Int
type DoubleFuncM = FuncM Double
type BoolFuncM = FuncM Bool
type AnyFuncM = FuncM CConst

data InterpretError where
  NoVariable :: Name -> String -> InterpretError
  NoFunc :: Name -> String -> InterpretError
  NoMainFunc :: String -> InterpretError
  IOError :: String -> InterpretError
  TypeMismatch :: CType -> CType -> String -> InterpretError
  
instance PrettyPrinter InterpretError where
  showPretty (NoVariable n extra) = "Cant find variable named " ++ n ++ "\n" ++ extra
  showPretty (NoFunc n extra) = "Cant find function named " ++ n ++ "\n" ++ extra
  showPretty (NoMainFunc extra) = "Cant find main functon\n" ++ extra
  showPretty (IOError extra) = "IO error\n" ++ extra
  showPretty (TypeMismatch a b extra) = "Type mismatch: expected " ++ showPretty a ++ " got " ++ showPretty b ++ "\n" ++ extra 

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
  
addVar :: Var -> ProgramState -> ProgramState
addVar var ps = ProgramState { vars = var : vars ps, funcs = funcs ps }

getValueInt :: Name -> ProgramState -> Int
getValueInt targetName st = helper $ vars st where
  helper [] = error "No such variable"
  helper (Var curName (CIntVal i) : tl) 
    | curName == targetName = i
    | otherwise = helper tl
  helper (Var curName _ : tl)
    | curName == targetName = error "Type missmatch"
    | otherwise = helper tl

getValue :: Name -> ProgramState -> CConst
getValue targetName = helper . vars where
  helper [] = error $ "No such variable"
  helper (Var curName res : tl)
    | curName == targetName = res
    | otherwise             = helper tl

getVarType :: Name -> ProgramState -> CType
getVarType targetName = helper . vars where
  helper [] = error "No such variable"
  helper (Var curName val : tl)
    | curName == targetName = getType val
    | otherwise             = helper tl
  getType (CStringVal _) = CString
  getType (CIntVal _) = CInt
  getType (CDoubleVal _) = CDouble
  getType (CBoolVal _) = CBool

makeArg :: ArgVar -> CConst -> Var
makeArg (ArgVar CString name) syn@(CStringVal _) = Var name syn
makeArg (ArgVar CInt name) syn@(CIntVal _) = Var name syn
makeArg (ArgVar CDouble name) syn@(CDoubleVal _) = Var name syn
makeArg (ArgVar CBool name) syn@(CBoolVal _) = Var name syn
makeArg _ _ = error "Type missmatch"

extractM :: [AnyFuncM] -> FuncM [CConst]
extractM [] = return []
extractM (e : es) = do
  e' <- e
  es' <- extractM es
  return (e' : es')

evalAnyExp :: Expr -> AnyFuncM
evalAnyExp (ExpName n) = do gets (getValue n)
evalAnyExp (ExpConstString s) = return $ CStringVal s
evalAnyExp (ExpConstInt i) = return $ CIntVal i
evalAnyExp (ExpConstDouble d) = return $ CDoubleVal d
evalAnyExp (ExpConstBool b) = return $ CBoolVal b
evalAnyExp (ExpFunCall name args) = do
  args' <- extractM $ fmap evalAnyExp args
  (ProgramState _ fs) <- get
  fun <- findFunc fs
  let tmp = callFunc fun args'
  res <- liftIO $ runExceptT $ evalStateT (sFuncBody fun) (ProgramState tmp fs)
  case res of
    syn@(Left _) -> liftEither syn
    (Right ok)   -> return ok
  where
    findFunc [] = throwError $ NoFunc name ""
    findFunc (f : fs)
      | sFuncName f == name = return f
      | otherwise           = findFunc fs
    callFunc fun args' = fmap (uncurry makeArg) (zip (sFuncArgs fun) args')
evalAnyExp (ExpCin vs) = cinHelper vs where
  cinHelper [] = return $ CBoolVal True
  cinHelper (n : tl) = do
    st <- get
    _ <- case getVarType n st of
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
    liftIO $ putStrLn $ showPretty e'
    coutHelper tl

evalAnyExp (ExpAssign n e) = setValue n e
evalAnyExp (a :+: b) = do
  a' <- evalAnyExp a
  b' <- evalAnyExp b
  return (a' + b')
evalAnyExp (a :-: b) = do
  a' <- evalAnyExp a
  b' <- evalAnyExp b
  return (a' - b')
evalAnyExp (a :*: b) = do
  a' <- evalAnyExp a
  b' <- evalAnyExp b
  return (a' * b')
evalAnyExp (a :/: b) = do
  a' <- evalAnyExp a
  b' <- evalAnyExp b
  return (a' / b')
evalAnyExp (a :<: b) = do
  a' <- evalAnyExp a
  b' <- evalAnyExp b
  return $ CBoolVal (a' < b')
evalAnyExp (a :>: b) = do
  a' <- evalAnyExp a
  b' <- evalAnyExp b
  return $ CBoolVal (a' > b')
evalAnyExp (a :!=: b) = do
  a' <- evalAnyExp a
  b' <- evalAnyExp b
  return $ CBoolVal (a' /= b')
evalAnyExp (a :==: b) = do
  a' <- evalAnyExp a
  b' <- evalAnyExp b
  return $ CBoolVal (a' == b')

evalStringExp :: Expr -> StringFuncM
evalStringExp e = do
  res <- evalAnyExp e
  case res of
      CStringVal s -> return s
      _ -> throwError $ TypeMismatch CInt CDouble "Not implemented"

evalIntExp :: Expr -> IntFuncM
evalIntExp e = do
  res <- evalAnyExp e
  case res of
      CIntVal i -> return i
      _ -> throwError $ TypeMismatch CInt CDouble "Not implemented"

evalDoubleExp :: Expr -> DoubleFuncM
evalDoubleExp e = do
  res <- evalAnyExp e
  case res of
      CDoubleVal d -> return d
      _ -> throwError $ TypeMismatch CInt CDouble "Not implemented"

evalBoolExp :: Expr -> BoolFuncM
evalBoolExp e = do
  res <- evalAnyExp e
  case res of
    CBoolVal b -> return b
    _ -> throwError $ TypeMismatch CInt CDouble "Not implemented"

evalVoidExp :: Expr -> VoidFuncM
evalVoidExp e = do
  _ <- evalAnyExp e
  return ()

addStringVar :: Name -> Expr -> VoidFuncM
addStringVar n e = do
  res <- evalStringExp e
  modify (addVar (Var n (CStringVal res)))

addIntVar :: Name -> Expr -> VoidFuncM
addIntVar n e = do
  res <- evalIntExp e
  modify (addVar (Var n (CIntVal res)))

addDoubleVar :: Name -> Expr -> VoidFuncM
addDoubleVar n e = do
  res <- evalDoubleExp e
  modify (addVar (Var n (CDoubleVal res)))

addBoolVar :: Name -> Expr -> VoidFuncM
addBoolVar n e = do
  res <- evalBoolExp e
  modify (addVar (Var n (CBoolVal res)))

makeAnyFunc :: Func -> AnyFuncM
makeAnyFunc (Func _ _ _ body) = helper body where
  helper [] = return $ CIntVal 0
  helper (Return e : tl) = evalAnyExp e
  helper (Val e : tl) = do
    evalVoidExp e
    helper tl
  helper (DiffString n e : tl) = do
    addStringVar n e
    helper tl
  helper (DiffInt n e : tl) = do
    addIntVar n e
    helper tl
  helper (DiffDouble n e : tl) = do
    addDoubleVar n e
    helper tl
  helper (DiffBool n e : tl) = do
    addBoolVar n e
    helper tl
  helper (If e thn Nothing : tl) = do
    cnd <- evalBoolExp e
    when cnd $ do
      _ <- helper thn
      return ()
    helper tl
  helper (If e thn (Just els) : tl) = do
    cnd <- evalBoolExp e
    _ <- if cnd then helper thn
                else helper els
    helper tl
  helper syn@(While cnd whileBody : tl) = do
    res <- evalBoolExp cnd
    if res then do
      _ <- helper whileBody
      helper syn
    else
      helper tl

makeStringFunc :: Func -> StringFuncM
makeStringFunc syn@(Func CString _ _ _) = do
  res <- makeAnyFunc syn
  case res of
    CStringVal s -> return s
    _         -> throwError $ TypeMismatch CInt CDouble "Not implemented"
makeStringFunc _ = throwError $ TypeMismatch CInt CDouble "Not implemented"

makeIntFuncM :: Func -> IntFuncM
makeIntFuncM syn@(Func CInt _ _ body) = helper body where
  helper [] = return 0
  helper _ = do
    res <- makeAnyFunc syn
    case res of
      CIntVal i -> return i
      _         -> throwError $ TypeMismatch CInt CDouble "Not implemented"
makeIntFuncM _ = throwError $ TypeMismatch CInt CDouble "Not implemented"

makeDoubleFunc :: Func -> DoubleFuncM
makeDoubleFunc syn@(Func CDouble _ _ _) = do
  res <- makeAnyFunc syn
  case res of
    CDoubleVal d -> return d
    _         -> throwError $ TypeMismatch CInt CDouble "Not implemented"
makeDoubleFunc _ = throwError $ TypeMismatch CInt CDouble "Not implemented"

makeBoolFunc :: Func -> BoolFuncM
makeBoolFunc syn@(Func CBool _ _ _) = do
  res <- makeAnyFunc syn
  case res of
    CBoolVal b -> return b
    _         -> throwError $ TypeMismatch CInt CDouble "Not implemented"
makeBoolFunc _ = throwError $ TypeMismatch CInt CDouble "Not implemented"

getMain :: [Func] -> IntFuncM
getMain [] = throwError $ TypeMismatch CInt CDouble "Not implemented"
getMain (f : fs)
  | funcName f == "main" = if funcType f == CInt then makeIntFuncM f else throwError $ TypeMismatch CInt CDouble "Not implemented"
  | otherwise            = getMain fs



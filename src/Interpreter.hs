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
  BadOperation :: String -> InterpretError
  TypeMismatch :: CType -> CType -> String -> InterpretError
  
instance PrettyPrinter InterpretError where
  showPretty (NoVariable n extra) = "Cant find variable named " ++ n ++ "\n" ++ extra
  showPretty (NoFunc n extra) = "Cant find function named " ++ n ++ "\n" ++ extra
  showPretty (NoMainFunc extra) = "Cant find main functon\n" ++ extra
  showPretty (IOError extra) = "IO error\n" ++ extra
  showPretty (TypeMismatch a b extra) = "Type mismatch: expected " ++ showPretty a ++ " got " ++ showPretty b ++ "\n" ++ extra 
  showPretty (BadOperation extra) = "Bad operation\n" ++ extra

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

getValue :: Name -> AnyFuncM
getValue targetName = do 
  t <- get
  helper $ vars t 
  where
    helper [] = throwError $ NoVariable targetName ""
    helper (Var curName res : tl)
      | curName == targetName = return res
      | otherwise             = helper tl

getVarType :: Name -> FuncM CType
getVarType targetName = do
  t <- get
  helper $ vars t
  where
    helper [] = throwError $ NoVariable targetName ""
    helper (Var curName val : tl)
      | curName == targetName = return $ getType val
      | otherwise             = helper tl
    getType (CStringVal _) = CString
    getType (CIntVal _) = CInt
    getType (CDoubleVal _) = CDouble
    getType (CBoolVal _) = CBool

makeArg :: ArgVar -> CConst -> Except InterpretError Var
makeArg (ArgVar CString name) syn@(CStringVal _) = return $ Var name syn
makeArg (ArgVar CInt name) syn@(CIntVal _) = return $ Var name syn
makeArg (ArgVar CDouble name) syn@(CDoubleVal _) = return $ Var name syn
makeArg (ArgVar CBool name) syn@(CBoolVal _) = return $ Var name syn
makeArg _ _ = throwError $ TypeMismatch CInt CDouble "Not implemented"

extractM :: [AnyFuncM] -> FuncM [CConst]
extractM [] = return []
extractM (e : es) = do
  e' <- e
  es' <- extractM es
  return (e' : es')
  


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
    callFunc fun args' = tt $ fmap (uncurry makeArg) (zip (sFuncArgs fun) args')
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
    liftIO $ putStrLn $ showPretty e'
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
  execBody (DiffString n e : tl) = do
    addStringVar n e
    execBody tl
  execBody (DiffInt n e : tl) = do
    addIntVar n e
    execBody tl
  execBody (DiffDouble n e : tl) = do
    addDoubleVar n e
    execBody tl
  execBody (DiffBool n e : tl) = do
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
  execBody syn@(While cnd whileBody : tl) = do
    res <- evalBoolExp cnd
    res' <- if res then do
      bodyRes <- execBody whileBody
      case bodyRes of
        Nothing -> execBody syn
        syn@(Just _) -> return syn
    else
      execBody tl 
    case res' of
      Nothing -> execBody tl
      syn@(Just _) -> return syn

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

(.+.) :: CConst -> CConst -> AnyFuncM
(.+.) (CStringVal a) (CStringVal b) = return $ CStringVal (a ++ b)
(.+.) (CIntVal a) (CIntVal b) = return $ CIntVal (a + b)
(.+.) (CDoubleVal a) (CDoubleVal b) = return $ CDoubleVal (a + b)
(.+.) (CBoolVal True) (CBoolVal _) = return $ CBoolVal True
(.+.) (CBoolVal _) (CBoolVal True) = return $ CBoolVal True
(.+.) (CBoolVal _) (CBoolVal _) = return $ CBoolVal False
(.+.) _ _ = throwError $ BadOperation "+"

(.*.) :: CConst -> CConst -> AnyFuncM
(.*.) (CIntVal a) (CIntVal b) = return $ CIntVal (a * b)
(.*.) (CDoubleVal a) (CDoubleVal b) = return $ CDoubleVal (a * b)
(.*.) (CBoolVal True) (CBoolVal True) = return $ CBoolVal True
(.*.) (CBoolVal _) (CBoolVal _) = return $ CBoolVal False
(.*.) _ _ = throwError $ BadOperation "*"

(.-.) :: CConst -> CConst -> AnyFuncM
(.-.) (CIntVal a) (CIntVal b) = return $ CIntVal (a - b)
(.-.) (CDoubleVal a) (CDoubleVal b) = return $ CDoubleVal (a - b)
(.-.) (CBoolVal True) (CBoolVal True) =return $  CBoolVal False
(.-.) (CBoolVal False) (CBoolVal False) = return $ CBoolVal False
(.-.) (CBoolVal _) (CBoolVal _) = return $ CBoolVal True
(.-.) _ _ = throwError $ BadOperation "-"

(./.) :: CConst -> CConst -> AnyFuncM
(./.) (CIntVal a) (CIntVal b) = return $ CIntVal (a `div` b)
(./.) (CDoubleVal a) (CDoubleVal b) = return $ CDoubleVal (a / b)
(./.) _ _ = throwError $ BadOperation "/"

(.<.) :: CConst -> CConst -> AnyFuncM
(.<.) (CIntVal a) (CIntVal b) = return $ CBoolVal $ a < b
(.<.) (CDoubleVal a) (CDoubleVal b) = return $ CBoolVal $ a < b
(.<.) _ _ = throwError $ BadOperation "<"

(.>.) :: CConst -> CConst -> AnyFuncM
(.>.) (CIntVal a) (CIntVal b) = return $ CBoolVal $ a > b
(.>.) (CDoubleVal a) (CDoubleVal b) = return $ CBoolVal $ a > b
(.>.) _ _ = throwError $ BadOperation ">"

(.==.) :: CConst -> CConst -> AnyFuncM
(.==.) (CIntVal a) (CIntVal b) = return $ CBoolVal $ a == b
(.==.) (CStringVal a) (CStringVal b) = return $ CBoolVal $ a == b
(.==.) (CDoubleVal a) (CDoubleVal b) = return $ CBoolVal $ a == b
(.==.) (CBoolVal a) (CBoolVal b) = return $ CBoolVal $ a == b
(.==.) _ _ = throwError $ BadOperation "=="

(.!=.) :: CConst -> CConst -> AnyFuncM
(.!=.) a b = do
  (CBoolVal res) <- a .==. b
  return $ CBoolVal $ not res
{-# LANGUAGE GADTs #-}

module Utils where

type Name = String

data CType = CString | CInt | CDouble | CBool
data CConst = CStringVal String | CIntVal Int | CDoubleVal Double | CBoolVal Bool deriving Show
data Var = Var {varName :: Name, varValue :: CConst} deriving Show
data ArgVar = ArgVar {argVarType :: CType, argVarName :: Name}

newtype ProgramState = ProgramState {vars :: [Var]} deriving Show

setValue :: String -> Var -> ProgramState -> ProgramState
setValue name newVar pState = ProgramState { vars = setValueHelper (vars pState) } where
  setValueHelper [] = []
  setValueHelper (var : tl)  
    | varName var == name = newVar : tl
    | otherwise           = var : setValueHelper tl
    
addVar :: Var -> ProgramState -> ProgramState
addVar var ps = ProgramState $ var : vars ps

data Expr where
  ExpName :: String -> Expr
  ExpConstString :: String -> Expr
  ExpConstInt :: Int -> Expr
  ExpConstDouble :: Double -> Expr
  ExpConstBool :: Bool -> Expr
  ExpFunCall :: String -> [Expr] -> Expr
  ExpCin :: [Name] -> Expr
  ExpCout :: [Expr] -> Expr
  ExpAssign :: Name -> Expr -> Expr
  (:+:) :: Expr -> Expr -> Expr
  (:-:) :: Expr -> Expr -> Expr
  (:*:) :: Expr -> Expr -> Expr
  (:/:) :: Expr -> Expr -> Expr
  (:==:) :: Expr -> Expr -> Expr
  (:!=:) :: Expr -> Expr -> Expr
  (:<:) :: Expr -> Expr -> Expr
  (:>:) :: Expr -> Expr -> Expr
      
data Line where
  Val :: Expr -> Line
  Return :: Expr -> Line
  DiffString :: Name -> Expr -> Line
  DiffInt :: Name -> Expr -> Line
  DiffDouble :: Name -> Expr -> Line
  DiffBool :: Name -> Expr -> Line
  If :: Expr -> Body -> Maybe Body -> Line
  While :: Expr -> Body -> Line 
  
data Func = Func {funcType :: CType, funcName :: String, funcArgs :: FuncArgs, funcBody :: Body}

type Body = [Line]
type FuncArgs = [ArgVar]
     
makeDiff :: CType -> (Name, Maybe Expr) -> Line
makeDiff CString (n, Nothing) = DiffString n (ExpConstString "")
makeDiff CInt (n, Nothing) = DiffInt n (ExpConstInt 0)
makeDiff CDouble (n, Nothing) = DiffDouble n (ExpConstDouble 0.0)
makeDiff CBool (n, Nothing) = DiffBool n (ExpConstBool False)
makeDiff CString (n, Just val) = DiffString n val
makeDiff CInt (n, Just val) = DiffInt n val
makeDiff CDouble (n, Just val) = DiffDouble n val
makeDiff CBool (n, Just val) = DiffBool n val

makeDiffList :: CType -> [(Name, Maybe Expr)] -> Body
makeDiffList t = fmap (makeDiff t)

--data Code = Return deriving Show
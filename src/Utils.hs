{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Utils where

type Name = String

data CType = CString | CInt | CDouble | CBool deriving (Show, Eq)
data CConst = CStringVal String | CIntVal Int | CDoubleVal Double | CBoolVal Bool deriving (Show, Eq)
data ArgVar = ArgVar {argVarType :: CType, argVarName :: Name} deriving (Show, Eq)

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
  
deriving instance Show Expr
deriving instance Show Line

deriving instance Eq Expr
deriving instance Eq Line  

data Func = Func {funcType :: CType, funcName :: String, funcArgs :: FuncArgs, funcBody :: Body} deriving (Show, Eq)

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

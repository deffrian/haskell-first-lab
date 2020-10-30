{-# LANGUAGE GADTs #-}

module Utils where

type Name = String

data CType = CString | CInt | CDouble | CBool deriving Eq
data CConst = CStringVal String | CIntVal Int | CDoubleVal Double | CBoolVal Bool deriving (Show, Eq)
data ArgVar = ArgVar {argVarType :: CType, argVarName :: Name}

instance Num CConst where
  (+) (CStringVal a) (CStringVal b) = CStringVal (a ++ b)
  (+) (CIntVal a) (CIntVal b) = CIntVal (a + b)
  (+) (CDoubleVal a) (CDoubleVal b) = CDoubleVal (a + b)
  (+) (CBoolVal True) (CBoolVal _) = CBoolVal True
  (+) (CBoolVal _) (CBoolVal True) = CBoolVal True
  (+) (CBoolVal _) (CBoolVal _) = CBoolVal False
  (+) _ _ = error "Cant perform op"
    
  (*) (CIntVal a) (CIntVal b) = CIntVal (a * b)
  (*) (CDoubleVal a) (CDoubleVal b) = CDoubleVal (a * b)
  (*) (CBoolVal True) (CBoolVal True) = CBoolVal True
  (*) (CBoolVal _) (CBoolVal _) = CBoolVal False
  (*) _ _ = error "Cant perform op"
  
  (-) (CIntVal a) (CIntVal b) = CIntVal (a - b)
  (-) (CDoubleVal a) (CDoubleVal b) = CDoubleVal (a - b)
  (-) (CBoolVal True) (CBoolVal True) = CBoolVal False
  (-) (CBoolVal False) (CBoolVal False) = CBoolVal False
  (-) (CBoolVal _) (CBoolVal _) = CBoolVal True
  (-) _ _ = error "Cant perform op"
    
instance Fractional CConst where
  (/) (CIntVal a) (CIntVal b) = CIntVal (a `div` b)
  (/) (CDoubleVal a) (CDoubleVal b) = CDoubleVal (a / b)
  (/) _ _ = error "Cant perform op"

instance Ord CConst where
  (<=) (CIntVal a) (CIntVal b) = a <= b
  (<=) (CDoubleVal a) (CDoubleVal b) = a <= b
  (<=) _ _ = error "Cant perform op"

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

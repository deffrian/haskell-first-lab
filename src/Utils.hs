{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Utils where

type Name = String

-- | c++ types
data CType = CString | CInt | CDouble | CBool deriving (Show, Eq)

-- | c++ const values
data CConst = CStringVal String | CIntVal Int | CDoubleVal Double | CBoolVal Bool deriving (Show, Eq)

-- | function argument
data ArgVar = ArgVar {argVarType :: CType, argVarName :: Name} deriving (Show, Eq)

-- | type for c++ expressions that have value
data Expr where
  -- | Variable name
  ExpName
    :: String -- ^ var name
    -> Expr

  -- | const string value
  ExpConstString
    :: String -- ^ value
    -> Expr

  -- | const int value
  ExpConstInt
    :: Int -- ^ value
    -> Expr

  -- | const double value
  ExpConstDouble
    :: Double -- ^ value
    -> Expr

  -- | const bool value
  ExpConstBool
    :: Bool -- ^ value
    -> Expr

  -- | function call
  ExpFunCall
    :: String -- ^ function name
    -> [Expr] -- ^ arguments
    -> Expr

  -- | c++ cin
  ExpCin
    :: [Name] -- ^ variables to read
    -> Expr

  -- | c++ cout
  ExpCout
    :: [Expr] -- ^ values to print
    -> Expr

  -- | variable assignment
  ExpAssign
    :: Name -- ^ variable name
    -> Expr -- ^ new value
    -> Expr

  -- | addition of two expressions
  (:+:) :: Expr -> Expr -> Expr

  -- | subtraction of two expressions
  (:-:) :: Expr -> Expr -> Expr

  -- | multiplication of two expressions
  (:*:) :: Expr -> Expr -> Expr

  -- | division of two expressions
  (:/:) :: Expr -> Expr -> Expr

  -- | is two expressions equal
  (:==:) :: Expr -> Expr -> Expr

  -- | is two expressions not equal
  (:!=:) :: Expr -> Expr -> Expr

  -- | is left expression less
  (:<:) :: Expr -> Expr -> Expr

  -- | is left expression greater
  (:>:) :: Expr -> Expr -> Expr

-- | type for c++ code line
data Line where
  -- | expression
  Val
    :: Expr -- ^ expression to evaluate
    -> Line

  -- | c++ return statement
  Return
    :: Expr -- ^ expression for return
    -> Line

  -- | new string variable
  DefString
    :: Name -- ^ variable name
    -> Expr -- ^ variable value
    -> Line

  -- | new int variable
  DefInt
    :: Name -- ^ variable name
    -> Expr -- ^ variable value
    -> Line

  -- | new double variable
  DefDouble
    :: Name -- ^ variable name
    -> Expr -- ^ variable value
    -> Line

  -- | new bool variable
  DefBool
    :: Name -- ^ variable name
    -> Expr -- ^ variable value
    -> Line

  -- | c++ if statement
  If
    :: Expr       -- ^ if condition
    -> Body       -- ^ if body
    -> Maybe Body -- ^ else body
    -> Line

  -- | c++ while statement
  While
    :: Expr -- ^ while condition
    -> Body -- ^ while body
    -> Line

deriving instance Show Expr
deriving instance Show Line

deriving instance Eq Expr
deriving instance Eq Line

-- | type for c++ functions
data Func = Func {funcType :: CType, funcName :: String, funcArgs :: FuncArgs, funcBody :: Body} deriving (Show, Eq)

-- | type for functions body
type Body = [Line]

-- | type for function arguments
type FuncArgs = [ArgVar]

-- | making c++ variable definition
makeDef :: CType -> (Name, Maybe Expr) -> Line
makeDef CString (n, Nothing) = DefString n (ExpConstString "")
makeDef CInt (n, Nothing) = DefInt n (ExpConstInt 0)
makeDef CDouble (n, Nothing) = DefDouble n (ExpConstDouble 0.0)
makeDef CBool (n, Nothing) = DefBool n (ExpConstBool False)
makeDef CString (n, Just val) = DefString n val
makeDef CInt (n, Just val) = DefInt n val
makeDef CDouble (n, Just val) = DefDouble n val
makeDef CBool (n, Just val) = DefBool n val

-- | making list of c++ variable definition
makeDefList :: CType -> [(Name, Maybe Expr)] -> Body
makeDefList t = fmap (makeDef t)

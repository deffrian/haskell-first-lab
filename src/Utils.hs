{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Utils where

import Control.Monad.State

data CType = CString | CInt | CDouble | CBool deriving Show
data CConst = CStringVal String | CIntVal Int | CDoubleVal Double | CBoolVal Bool deriving Show
data Var = Var {name :: String, value :: CConst} deriving Show

newtype ProgramState = ProgramState {vars :: [Var]} deriving Show

setValue :: String -> Var -> ProgramState -> ProgramState
setValue varName newVar pState = ProgramState { vars = setValueHelper (vars pState) } where
  setValueHelper [] = []
  setValueHelper (var : tl)  
    | name var == varName = newVar : tl
    | otherwise           = var : setValueHelper tl
    
addVar :: Var -> ProgramState -> ProgramState
addVar var ps = ProgramState $ var : vars ps

type Name = String

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


--data Code = Return deriving Show
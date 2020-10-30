{-# LANGUAGE FlexibleInstances #-}

module PrettyPrinter where

import Utils

showTabs :: Word -> String
showTabs 0 = ""
showTabs n = "  " ++ showTabs (n - 1)

class PrettyPrinter a where
  showPretty :: a -> String
  showPretty = showPrettyWithTabs 0
  showPrettyWithTabs :: Word -> a -> String
  showPrettyWithTabs n e = showTabs n ++ showPretty e

instance PrettyPrinter [Func] where
  showPretty [] = ""
  showPretty (e : tl) = showPretty e ++ "\n" ++ showPretty tl

instance PrettyPrinter a => PrettyPrinter (Maybe a) where
  showPretty Nothing = "Nothing"
  showPretty (Just a) = showPretty a

instance PrettyPrinter String where
  showPretty = show

instance PrettyPrinter Int where
  showPretty = show

instance PrettyPrinter Double where
  showPretty = show

instance PrettyPrinter Bool where
  showPretty True = "true"
  showPretty False = "false"

instance PrettyPrinter Func where
  showPretty (Func tp name args body) =
    showPretty tp ++ " " ++ name ++ "(" ++ showPretty args ++ ") {\n" ++
    showPrettyWithTabs 1 body ++ "}\n"
    
instance PrettyPrinter Line where
  showPrettyWithTabs tc (Val e) = showPrettyWithTabs tc e ++ ";"
  showPrettyWithTabs tc (Return e) = showTabs tc ++ "return " ++ showPretty e ++ ";"
  showPrettyWithTabs tc (DiffString n s) = showTabs tc ++ "string " ++ n ++ " = " ++ showPretty s ++ ";"
  showPrettyWithTabs tc (DiffInt n s) = showTabs tc ++ "int " ++ n ++ " = " ++ showPretty s ++ ";"
  showPrettyWithTabs tc (DiffDouble n s) = showTabs tc ++ "double " ++ n ++ " = " ++ showPretty s ++ ";"
  showPrettyWithTabs tc (DiffBool n s) = showTabs tc ++ "bool " ++ n ++ " = " ++ showPretty s ++ ";"
  showPrettyWithTabs tc (If cnd thn Nothing) =
    showTabs tc ++ "if (" ++ showPretty cnd ++ ") {\n" ++
    showPrettyWithTabs (tc + 1) thn ++
    showTabs tc ++ "}"
  showPrettyWithTabs tc (If cnd thn (Just els)) =
    showTabs tc ++ "if (" ++ showPretty cnd ++ ") {\n" ++
    showPrettyWithTabs (tc + 1) thn ++
    showTabs tc ++ "} else { \n" ++
    showPrettyWithTabs (tc + 1) els ++
    showTabs tc ++ "}"
  showPrettyWithTabs tc (While cnd body) =
    showTabs tc ++ "while (" ++ showPretty cnd ++ ") {\n" ++
    showPrettyWithTabs (tc + 1) body ++
    showTabs tc ++ "}"
    
instance PrettyPrinter Expr where
  showPretty (ExpName n) = n
  showPretty (ExpConstString s) = showPretty s
  showPretty (ExpConstInt i) = showPretty i
  showPretty (ExpConstDouble d) = showPretty d
  showPretty (ExpConstBool b) = showPretty b
  showPretty (ExpFunCall n es) = showPretty n ++ "(" ++ showFuncArgs es ++ ")" where
    showFuncArgs [] = ""
    showFuncArgs [lst] = showPretty lst
    showFuncArgs (e : tl) = showPretty e ++ "," ++ showFuncArgs tl
  showPretty (ExpCin vs) = "cin" ++ showCinArgs vs where
    showCinArgs [] = ""
    showCinArgs (e : tl) = " >> " ++ e ++ showCinArgs tl
  showPretty (ExpCout es) = "cout" ++ showCoutArgs es where
    showCoutArgs [] = ""
    showCoutArgs (e : tl) = " << " ++ showPretty e ++ showCoutArgs tl
  showPretty (ExpAssign n e) = n ++ " = " ++ showPretty e
  showPretty (a :+: b) = "(" ++ showPretty a ++ " + " ++ showPretty b ++ ")"
  showPretty (a :-: b) = "(" ++ showPretty a ++ " - " ++ showPretty b ++ ")"
  showPretty (a :*: b) = "(" ++ showPretty a ++ " * " ++ showPretty b ++ ")"
  showPretty (a :/: b) = "(" ++ showPretty a ++ " / " ++ showPretty b ++ ")"
  showPretty (a :==: b) = "(" ++ showPretty a ++ " == " ++ showPretty b ++ ")"
  showPretty (a :!=: b) = "(" ++ showPretty a ++ " != " ++ showPretty b ++ ")"
  showPretty (a :<: b) = "(" ++ showPretty a ++ " < " ++ showPretty b ++ ")"
  showPretty (a :>: b) = "(" ++ showPretty a ++ " > " ++ showPretty b ++ ")"
  
instance PrettyPrinter CType where
  showPretty CString = "string"
  showPretty CInt = "int"
  showPretty CDouble = "double"
  showPretty CBool = "bool"
  
instance PrettyPrinter Body where
  showPrettyWithTabs _ [] = ""
  showPrettyWithTabs n (l : ls) = showPrettyWithTabs n l ++ "\n" ++ showPrettyWithTabs n ls
  
instance PrettyPrinter FuncArgs where
  showPretty [] = ""
  showPretty [lst] = showPretty lst
  showPretty (e : tl) = showPretty e ++ ", " ++ showPretty tl

instance PrettyPrinter ArgVar where
  showPretty (ArgVar tp name) = showPretty tp ++ " " ++ name
  
instance PrettyPrinter CConst where
  showPretty (CStringVal s) = show s
  showPretty (CIntVal i) = show i
  showPretty (CDoubleVal d) = show d
  showPretty (CBoolVal True) = "true"
  showPretty (CBoolVal False) = "false"
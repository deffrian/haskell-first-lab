module Main where

import Lib
import CPPLexer as L
import Control.Monad.Identity as Id
import CPPParser as P
import Utils
import PrettyPrinter
import Control.Monad.State

{-program :: Func
program = do
  modify $ addVar $ Var "a" (CInt 1)
  modify $ addVar $ Var "b" (CInt 2)
  st <- get
  let var = head $ vars st
  return $ var
-}

tmp :: [Token] -> Maybe [Func]
tmp = P.cppParser

main :: IO ()
main = do
  x <- readFile "test/sample7.cpp"
  let res = tmp $ L.alexScanTokens x
  putStrLn $ showPrettyWithTabs 0 res

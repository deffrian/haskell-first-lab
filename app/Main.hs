module Main where

import Lib
import CPPLexer as L

main :: IO ()
main = do
  x <- readFile "test/sample4.cpp"
  let res = L.alexScanTokens x
  print res

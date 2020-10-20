module Main where

import Lib
import CPPLexer as L
import CPPParser as P

tmp :: [Token] -> Maybe Int
tmp = P.cppParser

main :: IO ()
main = do
  x <- readFile "test/sample5.cpp"
  let res = tmp $ L.alexScanTokens x
  print res

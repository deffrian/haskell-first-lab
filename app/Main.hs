module Main where

import Lib
import CPPLexer as L
import Control.Monad.Identity as Id
import CPPParser as P
import Utils
import PrettyPrinter
import Interpreter
import Control.Monad.State
import Control.Monad.Except

tmp :: [Token] -> Maybe [Func]
tmp = P.cppParser

main :: IO ()
main = do
  x <- readFile "test/sample5.cpp"
  case tmp $ L.alexScanTokens x of
    syn@(Just res) -> do
      let fs = fmap (\fn -> StateFunc (funcType fn) (funcName fn) (funcArgs fn) (makeAnyFunc fn)) res
      let mainFunc = getMain res
      res <- runExceptT $ evalStateT mainFunc (ProgramState{vars = [], funcs = fs})
      case res of
        (Left err) -> putStrLn $ showPretty err
        (Right ok) -> print ok
    Nothing -> putStrLn "Parse error"

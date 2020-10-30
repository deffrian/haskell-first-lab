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
import System.Environment

tmp :: [Token] -> Except P.ParseError [Func]
tmp = P.cppParser

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-p", fileName] -> do
      file <- readFile fileName
      case runExcept $ tmp $ L.alexScanTokens file of
        (Right ok) -> putStrLn $ showPretty ok
        (Left err) -> putStrLn $ showPretty err
    ["-i", fileName] -> do
      file <- readFile fileName
      case runExcept $ tmp $ L.alexScanTokens file of
        (Right res) -> do
          let fs = fmap (\fn -> StateFunc (funcType fn) (funcName fn) (funcArgs fn) (makeAnyFunc fn)) res
          let mainFunc = getMain res
          res' <- runExceptT $ evalStateT mainFunc (ProgramState{vars = [], funcs = fs})
          case res' of
            (Left err) -> putStrLn $ showPretty err
            (Right ok) -> putStrLn $ "\nExit code " ++ show ok
        (Left err) -> putStrLn $ showPretty err
    _ -> do
      putStrLn "Usage: "
      putStrLn "  -i file_name -- interpret file"
      putStrLn "  -p file_name -- print file"
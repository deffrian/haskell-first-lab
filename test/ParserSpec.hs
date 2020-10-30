module ParserSpec where

import qualified CPPParser as P
import qualified CPPLexer as L
import Utils
import Test.Hspec
import Control.Monad.Except

generateDefaultMain :: String -> String
generateDefaultMain s = "int main() { return " ++ s ++ "; }"

generateDefaultAns :: Expr -> [Func]
generateDefaultAns e = [Func CInt "main" [] [Return e]]

spec :: Spec
spec = do
  describe "Elementary" $ do
    it "return 0" $
      runExcept (P.cppParser $ L.alexScanTokens $ generateDefaultMain "0")
        `shouldBe` Right (generateDefaultAns (ExpConstInt 0))
    it "return 1 + 1" $ 
      runExcept (P.cppParser $ L.alexScanTokens $ generateDefaultMain "1 + 1")
        `shouldBe` Right (generateDefaultAns (ExpConstInt 1 :+: ExpConstInt 1))
    it "return 2 * 3 + 1" $
      runExcept (P.cppParser $ L.alexScanTokens $ generateDefaultMain "2 * 3 - 1")
        `shouldBe` Right (generateDefaultAns ((ExpConstInt 2 :*: ExpConstInt 3) :-: ExpConstInt 1))
    it "return -1" $
      runExcept (P.cppParser $ L.alexScanTokens $ generateDefaultMain "-1")
        `shouldBe` Right (generateDefaultAns (ExpConstInt (-1)))
    it "empty" $
      runExcept (P.cppParser $ L.alexScanTokens "int main() {}")
        `shouldBe` Right [Func CInt "main" [] []]
    it "double func" $
      runExcept (P.cppParser $ L.alexScanTokens "double main() {}")
        `shouldBe` Right [Func CDouble "main" [] []]
  describe "Errors" $ do
    it "no }" $
      runExcept (P.cppParser $ L.alexScanTokens "string foo() {")
        `shouldBe` Left P.UnexpectedEnd
    it "no ;" $
      runExcept (P.cppParser $ L.alexScanTokens "string foo() {return 0}")
        `shouldBe` Left (P.UnexpectedToken L.CloseBrace)
    it "bad func name" $
      runExcept (P.cppParser $ L.alexScanTokens "string int() {return 0;}")
        `shouldBe` Left (P.UnexpectedToken L.TypeInt)
    it "bad return" $
      runExcept (P.cppParser $ L.alexScanTokens "string foo() {return int;}")
        `shouldBe` Left (P.UnexpectedToken L.TypeInt)
  describe "Args" $ do
    it "int a" $
      runExcept (P.cppParser $ L.alexScanTokens "bool foo(int a) {}")
        `shouldBe` Right [Func CBool "foo" [ArgVar CInt "a"] []]
    it "string a, int b" $ 
      runExcept (P.cppParser $ L.alexScanTokens "bool foo(string a, int b) {}")
        `shouldBe` Right [Func CBool "foo" [ArgVar CString "a", ArgVar CInt "b"] []]
  describe "Operations vits vars" $ do
    it "new var" $ do
      runExcept (P.cppParser $ L.alexScanTokens "bool foo() {int a;}")
        `shouldBe` Right [Func CBool "foo" [] [DiffInt "a" (ExpConstInt 0)]]
      runExcept (P.cppParser $ L.alexScanTokens "bool foo() {int a, b;}")
        `shouldBe` Right [Func CBool "foo" [] [DiffInt "a" (ExpConstInt 0), DiffInt "b" (ExpConstInt 0)]]
      runExcept (P.cppParser $ L.alexScanTokens "bool foo() {int a, b = 1;}")
        `shouldBe` Right [Func CBool "foo" [] [DiffInt "a" (ExpConstInt 0), DiffInt "b" (ExpConstInt 1)]]
      runExcept (P.cppParser $ L.alexScanTokens "bool foo() {int a = 2, b;}")
        `shouldBe` Right [Func CBool "foo" [] [DiffInt "a" (ExpConstInt 2), DiffInt "b" (ExpConstInt 0)]]
    it "assign var" $ do
      runExcept (P.cppParser $ L.alexScanTokens "bool foo() {a=2;}")
        `shouldBe` Right [Func CBool "foo" [] [Val $ ExpAssign "a" (ExpConstInt 2)]]
      runExcept (P.cppParser $ L.alexScanTokens "bool foo() {var = a;}")
        `shouldBe` Right [Func CBool "foo" [] [Val $ ExpAssign "var" (ExpName "a")]]    
  describe "Conditions" $ do
    it "while" $ do
      runExcept (P.cppParser $ L.alexScanTokens "bool foo() {while(true){}}")
        `shouldBe` Right [Func CBool "foo" [] [While (ExpConstBool True) []]]
      runExcept (P.cppParser $ L.alexScanTokens "bool foo() {while(true){return 1;}}")
        `shouldBe` Right [Func CBool "foo" [] [While (ExpConstBool True) [Return $ ExpConstInt 1]]]
    it "if" $ do
      runExcept (P.cppParser $ L.alexScanTokens "bool foo() {if(true){}}")
        `shouldBe` Right [Func CBool "foo" [] [If (ExpConstBool True) [] Nothing]]
      runExcept (P.cppParser $ L.alexScanTokens "bool foo() {if(false){} else {}}")
        `shouldBe` Right [Func CBool "foo" [] [If (ExpConstBool False) [] (Just [])]]
  describe "IO" $ do
    it "cin" $ do
      runExcept (P.cppParser $ L.alexScanTokens "bool foo() {cin >> a;}")
        `shouldBe` Right [Func CBool "foo" [] [Val $ ExpCin ["a"]]]
      runExcept (P.cppParser $ L.alexScanTokens "bool foo() {cin >> a >> b;}")
        `shouldBe` Right [Func CBool "foo" [] [Val $ ExpCin ["a", "b"]]]
    it "cout" $ do
      runExcept (P.cppParser $ L.alexScanTokens "bool foo() {cout << a;}")
        `shouldBe` Right [Func CBool "foo" [] [Val $ ExpCout [ExpName "a"]]]
      runExcept (P.cppParser $ L.alexScanTokens "bool foo() {cout << 1 << true;}")
        `shouldBe` Right [Func CBool "foo" [] [Val $ ExpCout [ExpConstInt 1, ExpConstBool True]]]
{
module CPPParser where
import CPPLexer
import Data.Char
import Control.Monad.Identity
import Utils
import Data.List.NonEmpty (NonEmpty(..))
}

%name cppParser
%tokentype { Token }
%error { parseError }
%monad { Maybe } { thenE } { \x -> Just x }

%token
  'int'      { TypeInt }
  'double'   { TypeDouble }
  'bool'     { TypeBool }
  'string'   { TypeString }
  'while'    { While }
  'if'       { If }
  'else'     { Else }
  'cin'      { Cin }
  'cout'     { Cout }
  '<<'       { OpMvLeft }
  '>>'       { OpMvRight }
  '+'        { OpPlus }
  '-'        { OpSub }
  '*'        { OpMul }
  '/'        { OpDiv }
  '=='       { OpEq }
  '!='       { OpNotEq }
  '<'        { OpLess }
  '>'        { OpGreater }
  '('        { OpenBracket }
  ')'        { CloseBracket }
  '{'        { OpenBrace }
  '}'        { CloseBrace }
  ','        { Comma }
  ';'        { Semicolon }
  '='        { Assignment }
  'return'   { Return }
  valBool    { ValBool $$ }
  valStr     { ValString $$ }
  valInt     { ValInt $$ }
  valDouble  { ValDouble $$ }
  name       { Name $$ }
%%

Code:
  Func Code { 0 }
  | Func    { 0 }
Func:
  Type name '(' DeclArgs ')' '{' Body '}' { 0 }
DeclArgs:
  Type name ',' DeclArgs  { 0 }
  | Type name             { 0 }
  | {- empty -}           { 0 }
Body:
  Cycle Body       { 0 }
  | If Body        { 0 }
  | Diff ';' Body  { 0 }
  | AnyOp ';' Body { 0 }
  | {- empty -}    { 0 }
Cycle:
  'while' '(' Expr ')' '{' Body '}'  { 0 }
If:
  'if' '(' Expr ')' '{' Body '}'                        { 0 }
  | 'if' '(' Expr ')' '{' Body '}' 'else' '{' Body '}'  { 0 }
Diff:
  Type DiffNames          { 0 }
DiffNames:
  name { 0 }
  | name '=' Expr { 0 }
  | name ',' DiffNames { 0 }
  | name '=' Expr ',' DiffNames { 0 }
AnyOp:
  Expr            { 0 }
  | 'return' Expr { 0 }
Expr:
  name '=' Expr  { ExpAssign $1 $3 }
  | name         { ExpName $1 }
  | FuncCall     { $1 }
  | Cin          { $1 }
  | Cout         { $1 }
  | Const        { $1 }
  | BinOp        { $1 }
  | '(' Expr ')' { $2 }
BinOp:
  Expr '+' Expr    { ($1 :+: $3) }
  | Expr '-' Expr  { ($1 :-: $3) }
  | Expr '*' Expr  { ($1 :*: $3) }
  | Expr '/' Expr  { ($1 :/: $3) }
  | Expr '==' Expr { ($1 :==: $3) }
  | Expr '!=' Expr { ($1 :!=: $3) }
  | Expr '<' Expr  { ($1 :<: $3) }
  | Expr '>' Expr  { ($1 :>: $3) }
FuncCall:
  name '(' FuncArgs ')' { ExpFunCall $1 $3 }
Cin:
  'cin' InVars { ExpCin $2 }
Cout:
  'cout' OutValues { ExpCout $2 }
Const:
  valStr      { ExpConstString $1 }
  | valInt    { ExpConstInt $1 }
  | valDouble { ExpConstDouble $1 }
  | valBool   { ExpConstBool $1 }
OutValues:
  '<<' Expr OutValues { $2 : $3 }
  | '<<' Expr         { [$2] }
FuncArgs:
  Expr ',' FuncArgs { $1 : $3 }
  | Expr           { [$1] }
  | {- empty -}    { [] }
Type:
  'double'   { CDouble }
  | 'int'    { CInt }
  | 'bool'   { CBool }
  | 'string' { CString }
InVars:
  '>>' name InVars { $2 : $3 }
  | '>>' name      { [$2] }

{
parseError :: [Token] -> Maybe a
parseError tkns = Nothing

thenE :: Maybe a -> (a -> Maybe b) -> Maybe b
m `thenE` k =
  case m of
    Just a -> k a
    Nothing -> Nothing
}
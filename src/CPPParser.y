{
module CPPParser where
import qualified CPPLexer as Lex
import Data.Char
import Control.Monad.Identity
import Utils
import Data.List.NonEmpty (NonEmpty(..))
}

%name cppParser
%tokentype { Lex.Token }
%error { parseError }
%monad { Maybe } { thenE } { \x -> Just x }

%token
  'int'      { Lex.TypeInt }
  'double'   { Lex.TypeDouble }
  'bool'     { Lex.TypeBool }
  'string'   { Lex.TypeString }
  'while'    { Lex.While }
  'if'       { Lex.If }
  'else'     { Lex.Else }
  'cin'      { Lex.Cin }
  'cout'     { Lex.Cout }
  '<<'       { Lex.OpMvLeft }
  '>>'       { Lex.OpMvRight }
  '+'        { Lex.OpPlus }
  '-'        { Lex.OpSub }
  '*'        { Lex.OpMul }
  '/'        { Lex.OpDiv }
  '=='       { Lex.OpEq }
  '!='       { Lex.OpNotEq }
  '<'        { Lex.OpLess }
  '>'        { Lex.OpGreater }
  '('        { Lex.OpenBracket }
  ')'        { Lex.CloseBracket }
  '{'        { Lex.OpenBrace }
  '}'        { Lex.CloseBrace }
  ','        { Lex.Comma }
  ';'        { Lex.Semicolon }
  '='        { Lex.Assignment }
  'return'   { Lex.Return }
  valBool    { Lex.ValBool $$ }
  valStr     { Lex.ValString $$ }
  valInt     { Lex.ValInt $$ }
  valDouble  { Lex.ValDouble $$ }
  name       { Lex.Name $$ }

  %left '==' '!=' '<' '>'
  %left '+' '-'
  %left '*' '/'
%%



Code:
  Func Code { $1 : $2 }
  | Func    { [$1] }
Func:
  Type name '(' DeclArgs ')' '{' Body '}' { Func $1 $2 $4 $7 }
DeclArgs:
  Type name ',' DeclArgs  { ArgVar $1 $2 : $4 }
  | Type name             { [ArgVar $1 $2] }
  | {- empty -}           { [] }
Body:
  Cycle Body       { $1 : $2 }
  | If Body        { $1 : $2 }
  | Diff ';' Body  { $1 ++ $3 }
  | AnyOp ';' Body { $1 : $3 }
  | {- empty -}    { [] }
Cycle:
  'while' '(' Expr ')' '{' Body '}'  { While $3 $6 }
If:
  'if' '(' Expr ')' '{' Body '}'                        { If $3 $6 Nothing }
  | 'if' '(' Expr ')' '{' Body '}' 'else' '{' Body '}'  { If $3 $6 (Just $10) }
Diff:
  Type DiffNames          { makeDiffList $1 $2 }
DiffNames:
  name { [($1, Nothing)] }
  | name '=' Expr { [($1, Just $3)] }
  | name ',' DiffNames { ($1, Nothing) : $3 }
  | name '=' Expr ',' DiffNames { ($1, Just $3) : $5 }
AnyOp:
  Expr            { Val $1 }
  | 'return' Expr { Return $2 }
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
parseError :: [Lex.Token] -> Maybe a
parseError tkns = Nothing

thenE :: Maybe a -> (a -> Maybe b) -> Maybe b
m `thenE` k =
  case m of
    Just a -> k a
    Nothing -> Nothing
}
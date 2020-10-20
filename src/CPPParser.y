{
module CPPParser where
import CPPLexer
import Data.Char
}

%name cppParser
%tokentype { Token }
%error { parseError }

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
  Func Code { Nothing }
  | Func    { Nothing }
Func:
  Type name '(' DeclArgs ')' '{' Body '}' { Nothing }
DeclArgs:
  Type name ',' DeclArgs  { Nothing }
  | Type name         { Nothing }
  | {- empty -}       { Nothing }
Body:
  Cycle Body      { Nothing }
  | If Body       { Nothing }
  | Diff ';' Body { Nothing }
  | AnyOp ';' Body   { Nothing }
  | {- empty -}   { Nothing }
Cycle:
  'while' '(' ValOp ')' '{' Body '}'  { Nothing }
If:
  'if' '(' ValOp ')' '{' Body '}'                        { Nothing }
  | 'if' '(' ValOp ')' '{' Body '}' 'else' '{' Body '}'  { Nothing }
Diff:
  Type DiffNames          { Nothing }
DiffNames:
  name { Nothing }
  | name '=' ValOp { Nothing }
  | name ',' DiffNames { Nothing }
  | name '=' ValOp ',' DiffNames { Nothing }
AnyOp:
  ValOp            { Nothing }
  | 'return' ValOp { Nothing }
ValOp:
  name '=' ValOp  { Nothing }
  | name          { Nothing }
  | FuncCall      { Nothing }
  | Cin           { Nothing }
  | Cout          { Nothing }
  | Const         { Nothing }
  | BinOp         { Nothing }
  | '(' ValOp ')' { Nothing }
BinOp:
  ValOp '+' ValOp    { Nothing }
  | ValOp '-' ValOp  { Nothing }
  | ValOp '*' ValOp  { Nothing }
  | ValOp '/' ValOp  { Nothing }
  | ValOp '==' ValOp { Nothing }
  | ValOp '!=' ValOp { Nothing }
  | ValOp '<' ValOp  { Nothing }
  | ValOp '>' ValOp  { Nothing }
FuncCall:
  name '(' ValArgs ')' { Nothing }
Cin:
  'cin' InVars { Nothing }
InVars:
  '>>' name InVars { Nothing }
  | '>>' name      { Nothing }
Cout:
  'cout' OutValues { Nothing }
OutValues:
  '<<' AnyOp OutValues { Nothing }
  | '<<' AnyOp         { Nothing }
ValArgs:
  name ',' ValArgs    { Nothing }
  | Const ',' ValArgs { Nothing }
  | name              { Nothing }
  | Const             { Nothing }
  | {- empty -}       { Nothing }
Const:
  valStr      { Nothing }
  | valInt    { Nothing }
  | valDouble { Nothing }
  | valBool   { Nothing }
Type:
  'double'   { Nothing }
  | 'int'    { Nothing }
  | 'bool'   { Nothing }
  | 'string' { Nothing }

{
parseError :: [Token] -> a
parseError = error . show


}
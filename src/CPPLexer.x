{
module CPPLexer where
}

%wrapper "basic"

tokens :-
    $white+      ;
    \n           ;
    "int"             { \_ -> TypeInt }
    "double"          { \_ -> TypeDouble }
    "bool"            { \_ -> TypeBool }
    "string"          { \_ -> TypeString }
    "while"           { \_ -> While }
    "if"              { \_ -> If }
    "else"            { \_ -> Else }
    "cin"             { \_ -> Cin }
    "cout"            { \_ -> Cout }
    "<<"              { \_ -> OpMvLeft }
    ">>"              { \_ -> OpMvRight }
    "+"               { \_ -> OpPlus }
    "-"               { \_ -> OpSub }
    "*"               { \_ -> OpMul }
    "/"               { \_ -> OpDiv }
    "=="              { \_ -> OpEq }
    "!="              { \_ -> OpNotEq }
    "<"               { \_ -> OpLess }
    ">"               { \_ -> OpGreater }
    "("               { \_ -> OpenBracket }
    ")"               { \_ -> CloseBracket }
    "{"               { \_ -> OpenBrace }
    "}"               { \_ -> CloseBrace }
    ","               { \_ -> Comma }
    ";"               { \_ -> Semicolon }
    "="               { \_ -> Assignment }
    "true"            { \_ -> ValBool True }
    "false"           { \_ -> ValBool False }
    "return"          { \_ -> Return }
    \".*\"            { ValString . tail . init }
    [0-9]+            { ValInt . read }
    [0-9]+\.[0-9]+    { ValDouble . read }
    [a-zA-Z]+         { Name }
{

data Token = TypeInt | TypeDouble | TypeBool | TypeString |
             While | If | Else |
             Cin | Cout | OpMvLeft | OpMvRight |
             OpPlus | OpSub | OpMul | OpDiv | OpEq | OpNotEq | OpLess | OpGreater |
             OpenBracket | CloseBracket |
             OpenBrace | CloseBrace |
             Comma | Semicolon | Assignment |
             ValString String | ValInt Int | ValDouble Double | ValBool Bool |
             Return | Name String deriving (Eq, Show)

}
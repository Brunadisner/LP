{
module Parser where

import Lexer
}

%name parser
%name parser
%tokentype {Token}
%error {parseError}

%token
    num     { TokenNum $$ }
    '+'     { TokenAdd }
    '&'     { TokenAnd }
    "true"  { TokenTrue } 
    "false" { TokenFalse }
    "if"    { TokenIf }
    "then"  { TokenThen }
    "else"  { TokenElse }
%%
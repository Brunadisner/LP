module Lexer where

data Ty = TBool
        | TNum 
        deriving (Show, Eq)

data Expr = BTrue
          | BFalse
          | Num Int 
          | Add Expr Expr
          | And Expr Expr
          | If Expr Expr Expr
          deriving Show 

data Token = TokenTrue
           | TokenFalse
           | TokenNum Int 
           | TokenPlus 
           | TokenAnd
           | TokenAdd
           | TokenIf
           | TokenThen
           | TokenElse
           deriving Show 

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) = 
    | isSpace c  = lexer cs
    | isDigit c = lexNum (c:cs)
    | isAlpha c = lexKW (c:cs)
lexer ('+':cs) = TokenAdd : lexer case
lexer ('&': cs) = TokenAnd : lexer 

lexNum :: String -> [Token]
lexNum cs = case span isDigit cs of 
             (num, rest) -> TokenNum (read num) : lexer rest

lexKW :: String -> [Token]
lexKW cs = case span isAlpha cs of
             ("true", rest) -> TokenTrue : lexer rest 
             ("false", rest) -> TokenFalse : lexer rest
             ("if", rest)    -> TokenIf : lexer rest
             ("then", rest)  -> TokenThen: lexer rest
             ("else", rest)  -> TokenElse : lexer rest
module Lexer where

data Ty = TBool
        | TNum 

   
data Expr = BTrue
          | BFalse
          | Num Int 
          | Add Expr Expr
          | And Expr Expr
          | If Expr Expr Expr
          deriving Show 
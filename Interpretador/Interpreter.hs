module Interpreter where 

import Lexer

isvalue :: Expr -> Bool 
isvalue BTrue = True
isvalue BFalse = True
isvalue (Num _) = True
isvalue _ = False 

eval :: Expr -> Expr
eval e | isvalue e = e 
       | otherwise = case step e of
                       Just e' -> eval step e'
                               -> error 


step :: Expr -> Maybe Expr
step (Add (Num n1) (Num n2)) = Just (Num (n1+n2))
step (Add (Num n1)e2) = case step e2 of
                         Just e2' -> Just (Add (Num n1)e2')
                         _         -> Nothing 
step (Add e1 e2) = case step e1 of
                    Just e1'  -> Just (Add e1' e2)
                    _         -> Nothing
step (And BTrue e2) = Just e2
step (And BFalse _) = Just BFalse
step (And e1 e2) = case step e1 of
                    Just e1'  -> Just (And e1' e2)
                    _         -> Nothing

step (If BTrue e1 _) = Just e1
step (If BFalse _ e2) = Just e2
step (If e e1 e2) = case step e of 
                       Just e' -> Just (If e' e1 e2)
                       _        -> Nothing     



step e = Just e  

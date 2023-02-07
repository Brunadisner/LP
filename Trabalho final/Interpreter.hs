module Interpreter where

import Lexer

subst :: String -> Expr -> Expr -> Expr 
subst x n b@(Var v) = if v == x then 
                        n 
                      else 
                        b 
subst x n (Lam v t b) = Lam v t (subst x n b)
subst x n (App e1 e2) = App (subst x n e1) (subst x n e2)
subst x n (Add e1 e2) = Add (subst x n e1) (subst x n e2)
subst x n (And e1 e2) = And (subst x n e1) (subst x n e2)
subst x n (Sub e1 e2) = Sub (subst x n e1) (subst x n e2)
subst x n (Mul e1 e2) = Mul (subst x n e1) (subst x n e2)
subst x n (Maior e1 e2) = Maior (subst x n e1) (subst x n e2)
subst x n (Igual e1 e2) = Igual (subst x n e1) (subst x n e2)
subst x n (Menor e1 e2) = Menor (subst x n e1) (subst x n e2)
subst x n (If e e1 e2) = If (subst x n e) (subst x n e1) (subst x n e2)
subst x n (Paren e) = Paren (subst x n e)
subst x n (Eq e1 e2) = Eq (subst x n e1) (subst x n e2)
subst x n (Let v e1 e2) = Let v (subst x n e1) (subst x n e2)
subst x n e = e 

isvalue :: Expr -> Bool 
isvalue BTrue = True
isvalue BFalse = True
isvalue (Num _) = True
isvalue (Lam _ _ _) = True 
isvalue _ = False 

alti :: Expr -> Int
alti (Num x) = x




step :: Expr -> Maybe Expr 
step (Add (Num n1) (Num n2)) = Just (Num (n1 + n2))
step (Add (Num n1) e2) = case step e2 of 
                           Just e2' -> Just (Add (Num n1) e2')
                           _        -> Nothing
step (Add e1 e2) = case step e1 of 
                     Just e1' -> Just (Add e1' e2)
                     _        -> Nothing 

                     
step (Sub (Num n1) (Num n2)) = Just (Num (n1 - n2))
step (Sub (Num n1) e2) = case step e2 of 
                           Just e2' -> Just (Sub (Num n1) e2')
                           _        -> Nothing
step (Sub e1 e2) = case step e1 of 
                     Just e1' -> Just (Sub e1' e2)
                     _        -> Nothing 

step (Mul (Num n1) (Num n2)) = Just (Num (n1 * n2))
step (Mul (Num n1) e2) = case step e2 of 
                           Just e2' -> Just (Mul (Num n1) e2')
                           _        -> Nothing
step (Mul e1 e2) = case step e1 of 
                     Just e1' -> Just (Mul e1' e2)
                     _        -> Nothing 

                  
step (And BTrue e2) = Just e2 
step (And BFalse _) = Just BFalse 
step (And e1 e2) = case step e1 of 
                     Just e1' -> Just (And e1' e2)
                     _        -> Nothing
step (If BTrue e1 _) = Just e1 
step (If BFalse _ e2) = Just e2 
step (If e e1 e2) = case step e of 
                      Just e' -> Just (If e' e1 e2)
                      _       -> Nothing
step (App e1@(Lam x t b) e2) | isvalue e2 = Just (subst x e2 b)
                             | otherwise = case step e2 of 
                                             Just e2' -> Just (App e1 e2')
                                             _        -> Nothing 
step (Let x e1 e2) | isvalue e1 = Just (subst x e1 e2)
                             | otherwise = Let x (step e1) e2

step (App e1 e2) = case step e1 of 
                     Just e1' -> Just (App e1' e2)
                     _        -> Nothing
step (Paren e) = Just e
step (Eq e1 e2) | isvalue e1 && isvalue e2 = if (e1 == e2) then
                                               Just BTrue 
                                             else 
                                               Just BFalse 
                | isvalue e1 = case step e2 of 
                                 Just e2' -> Just (Eq e1 e2')
                                 _        -> Nothing
                | otherwise = case step e1 of 
                                Just e1' -> Just (Eq e1' e2)
                                _        -> Nothing 

step (Maior e1 e2) | isvalue e1 && isvalue e2 = if ((alti e1) > (alti e2)) then
                                               Just BTrue 
                                             else 
                                               Just BFalse 
                | isvalue e1 = case step e2 of 
                                 Just e2' -> Just (Maior e1 e2')
                                 _        -> Nothing
                | otherwise = case step e1 of 
                                Just e1' -> Just (Maior e1' e2)
                                _        -> Nothing

step (Menor e1 e2) | isvalue e1 && isvalue e2 = if ((alti e1) < (alti e2)) then
                                               Just BTrue 
                                             else 
                                               Just BFalse 
                | isvalue e1 = case step e2 of 
                                 Just e2' -> Just (Menor e1 e2')
                                 _        -> Nothing
                | otherwise = case step e1 of 
                                Just e1' -> Just (Menor e1' e2)
                                _        -> Nothing 

step (Igual e1 e2) | isvalue e1 && isvalue e2 = if ((alti e1) == (alti e2)) then
                                               Just BTrue 
                                             else 
                                               Just BFalse 
                | isvalue e1 = case step e2 of 
                                 Just e2' -> Just (Igual e1 e2')
                                 _        -> Nothing
                | otherwise = case step e1 of 
                                Just e1' -> Just (Igual e1' e2)
                                _        -> Nothing                                 

step (MrIgual e1 e2) | isvalue e1 && isvalue e2 = if ((alti e1) >= (alti e2)) then
                                               Just BTrue 
                                             else 
                                               Just BFalse 
                | isvalue e1 = case step e2 of 
                                 Just e2' -> Just (MrIgual e1 e2')
                                 _        -> Nothing
                | otherwise = case step e1 of 
                                Just e1' -> Just (MrIgual e1' e2)
                                _        -> Nothing         

step e = Just e 

eval :: Expr -> Expr 
eval e | isvalue e = e 
       | otherwise = case step e of 
                       Just e' -> eval e'
                       _       -> error "Interpreter error!"

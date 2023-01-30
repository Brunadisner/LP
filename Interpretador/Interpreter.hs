module Interpreter where 

import Lexer

subst :: String -> Expr -> Expr -> Expr 
subst x n b@(Var v) = if v == x then 
                         n
                        else
                          b
subst x n (Lam v b) = Lam v (subst x n b)
subst x n (App e1 e2) = App (subst x n e1) (subst x n e2)
subst x n (Add e1 e2) = Add (subst x n e1) (subst x n e2)
subst x n (And e1 e2) = And (subst x n e1) (subst x n e2)
subst x n (If e e1 e2) = If (subst x n e) (subst x n e1) (subst x n e2)
subst x n e = e







isvalue :: Expr -> Bool 
isvalue BTrue = True
isvalue BFalse = True
isvalue (Num _) = True
isvalue (Lam _) = True
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

step (App el@(Lam x b) e2) | isvalue e2 = subst x e1 e2
                           | otherwise = case step e2 of 
                                           Just e2' -> App e1 e2'
                                           _        -> Nothing     


step (App e1 e2) = case step e1 of 
                     Just e1' -> App e1' e2
                     _        ->  
step e = Just e  

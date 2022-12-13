module TypeCheker where 

import Lexer 

typeof :: Expr -> Maybe Ty 
typeof BTrue = Just TBool
typeof BFalse = Just TBool
typeof (Num _) = Just TNum
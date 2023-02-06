module Main where 

import Lexer
import Parser 
import TypeChecker
import Interpreter 

--main = getContents >>= print . eval . typecheck . parser . lexer
main = getContents >>= print . eval . typecheck . parser . lexer

-- lexer - parser e Typechecker 

-- Quando já é um valor usa a função subst e isvalue precisa substitui 
-- let x = 1 in x+1 -  no caso aqui precisa substituir o primeiro x

-- quando ainda não é valor precisa converter olhar do cálculo lambda 
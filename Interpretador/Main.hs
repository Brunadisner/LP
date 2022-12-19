module Main where 

import Lexer 
import Parser
import TypeCheker
import Interpreter


main = getContents >>= print . eval . typecheck . parser . lexer
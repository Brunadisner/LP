import Prelude ()
 


main :: IO ()
main = do putStrln "Digite a sua nota: "
          n1 <- readln 
          putStrln "Digite a sua nota: "
          n2 <- readln
          putStrln "Digite a sua nota: "
          n3 <- readln
          putStrln "Media das notas: "
          putStrln (show (n1+n2+n2/3))

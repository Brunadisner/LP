module Aula6 where -- o nome do modulo precisa iniciar com letra maiuscula 


dobro x = x + x

quadruplo x = dobro (dobro x)

areaCirculo r = pi *r^2

vabs :: Integer -> Integer 
vabs n | n >= 0 = n
       | n < 0 = -n

sinal :: Int -> Int 
sinal n | n > 0     = 1
        | n == 0    = 0
        | otherwise = -1

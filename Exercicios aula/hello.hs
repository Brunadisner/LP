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


count_lista :: [a] -> Int
count_lista l = foldl (\x y -> x + 1) 0 l


-- case not (1 /= 2) of
--         True -> "inicio"
--         False -> "fim"
teste x = if 2 == 2 then "abc" else ['d', 'e', 'f']

logt x = (map (logBase 4) [16, 256, 4096])

filter (not . odd . (+3)) [5, 6, 7, 8, 9]


-- let pot_dois x | x <= 0 = 1
--    | otherwise = 2 * pot_dois (x-1)
--   in pot_dois 7

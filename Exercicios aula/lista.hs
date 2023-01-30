module Lista where

-- Exercicio 1
exer1 salario = salario+(salario*0.1)-(salario*0.07) 

-- Exercicio 2
triangulo :: Int -> Int -> Int -> Bool
triangulo a b c | a+b > c = True
                | a+c > b = True 
                |c+b > a = True
                | otherwise = False 


-- Exercicio 3
conceito :: Float -> Float -> Float -> Char
conceito a b c 
    | s >= 8.0 = "A"
    | s >= 7.0 = "B"
    | s >= 6.0 = "C"
    | s >= 5.0 = "D"
    | s >= 0.0 = "E"
    where
    s = ((a * 0.2) + (b * 0.3) + (c * 0.5))
-- Exercicio 4

precoRetrato :: Integer -> String -> Double 
precoRetrato q d 
            | q == 1 = 100.00 + s
            | q == 2 = 130.00 + s
            | q == 3 = 150.00 + s
            | q == 4 = 165.00 + s 
            | q == 5 = 175.00 + s
            | q == 6 = 180.00 + s
            | q >= 7 = 185.00 + s
    where
            s = if d == "sabado" || d == "domingo"
                then ((precoRetrato q "*" )* 0.2)
                else 0.00

-- Exercio 5 

produto :: Float -> Float -> Float -> Float 
produto a b c = a*b*c

-- Exercicio 6

imc :: Float -> String -> Float 
imc h s | s == "feminino" = 62.1 * h - 44.7
        | s == "masculino" = 72.7 * h - 58

-- Exercicio 7 
media :: Float -> Float -> Float -> String
media a b c
            | n > 7 = "aprovado"
            | n > 3 && n < 7 = "exame especial"
            | n < 3 = "reprovado"
            where 
            n = (a + b + c) / 3

-- Exercicio 8
palindromo :: String -> Bool
palindromo d = if reverse d == d 
            then True
            else False

-- Exercicio 9 
emprestimo :: Float -> Float -> Bool
emprestimo sal prest = if prest > (sal*0.30) 

-- Exercicio 10 
-- eleitor :: Integer -> String
-- eleitor x 
--     | x < 16 = "não eleitor"
--     | x >= 18 && x < 65 = "eleitor obrigatório"
--     | otherwise = "eleitor facultativo"

--Exercicio 11
pot_dois :: Integer -> Float 
pot_dois let pot_dois x | x <= 0 = 1 
                        | otherwise = 2* pot_dois(x-1)
         in pot_dois 7

-- Exercicio 12
-- power :: Integer -> Integer -> Integer
-- power x 0 = 1
-- power x e = x * (power x (e - 1)) -- Código do professor


-- Exercicio 13
career :: Double -> Double -> Int -> Int -> Double -- código do professor 
career s a ac aa = if (ac == aa) then 
                     s
                   else
                        career(sal_inc s a)(2 * a)(ac + 1) aa
      where
        sal_inc s p = s + s * p/100


-- Exercicio 14
ultimo :: [Int] -> Int 
ultimo [] = 0
ultimo lst  = head (reverse lst)

-- Exercicio 15
primeiro :: [Integer] -> [Integer] 
primeiro [] = []
primeiro lst  = reverse(tail(reverse lst))

-- Exercicio 16
soma :: [Integer]-> Integer
soma list = sum list

-- Exercicio 17
produtoL :: [Int] -> [Int] -> [Int]
produtoL [][] = []
produtoL (x:xs) (y:ys) = (x*y) : produtoL xs ys


-- Exercicio 18
data Produto = Perecivel Int String Int Bool Comercio
            | Nperecivel Int String String Int  Comercio
            deriving Show

data Comercio = Un | Peso 
                deriving Show

prod1 = Perecivel 1 "Carne moída" 2022 True Peso

-- Exercicio 20
is_valid :: Produto -> Int -> Bool 
is_valid (Perecivel _ _ av_ _) aa = av >= aa -- patern cada anderline é um dado (usa para ignorar o dado)
is_valid _ _ = True

-- Exercicio 21


-- Exercicio 22


-- Exericicio 23
count_lista :: [a] -> Int
count_lista l = foldl (\x y -> x + 1) 0 l


-- Exercicio 24
comp' :: [Int] -> Int 
comp' [] = 0
comp' (x:xs) = 1 + comp' xs
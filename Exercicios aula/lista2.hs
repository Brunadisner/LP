module Lista2 where 

-- Exercicio 1
salario s = s+(s*0.1)-(s*0.07)

-- Exercicio 2
triangulo :: Int->Int->Int->Bool
triangulo a b c | a + b > c = True
                | b + c > a = True
                | a + c > b = True 
                | otherwise = False 


--Exercicio 3
media :: Float->Float->Float->String
media n1 n2 n3 | 8.0 < s && s >= 10.0 = "A"
               | 7.0 < s && s > 8.0 = "B"
               | 6.0 < s && s > 7.0 = "C"
               | 5.0 < s && s > 6.0 = "D"
               | 0.0 < s && s > 5.0 = "E"
                where s = ((n1*0.2)+(n2*0.3)+(n3*0.5))

--Exercicio 6
imc :: String -> Double -> Double 
imc s h | s == "Feminino" = (62.1 * h) - 44.7
        | s == "Masculino" = (72*7 * h) - 58

situacaoMedia :: Float -> Float -> Float -> String 
situacaoMedia a b c | m >= 7 = "Aprovado"
                    | m >= 3 && m < 7 = "Exame especial"
                    | m < 3 = "Reprovado"
                    where m = (a+b+c)/3

rever :: String -> Bool
rever s = if reverse s == s 
        then True 
        else False 

fatorialDuplo :: Integer -> Integer
fatorialDuplo 0 = 1
fatorialDuplo 1 = 1 
fatorialDuplo x = x * fatorialDuplo (x-2)

pot :: Integer -> Integer -> Integer 
pot x 0 = 1
pot x e = x * (pot x (e-1))

ultimo :: [a]->a
ultimo l = head (reverse l)


primeiro :: [a]->[a]
primeiro l = (reverse (tail (reverse l)) )

soma :: Num a => [a]->a
soma [] = 0
soma (x:xs) = x + soma xs

produto :: Num a => [a]->[a]->[a]
produto (x:xs) (y:ys) = (x * y) : produto xs ys 

data Produto = Perecivel Int String Int Bool Comercio 
             | Nperecivel Int String String Int Comercio
             deriving Show
        
data Comercio = Un | Peso 
               deriving Show

prod1 = Nperecivel 1 "Acucar" "sei la" 2022 Un 
prod2 = Perecivel 2 "Queijo" 2020 True Peso

valido :: Produto -> Int -> Bool
valido (Perecivel _ _ val _ _) aa = val >= aa
valido _ _ = True

list :: Num a => [a] -> a 
list [x] = x
list [] = 0
list (x:xs) = x + head xs

type Contact = (String,String)
contatos :: [Contact]
contatos = [("Bruna","499851154"),("Ana","87744564")]

filterC :: [Contact] -> String -> [Contact]
filterC l n = filter (\dados -> fst dados == n) l
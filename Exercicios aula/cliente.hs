module Cliente where 

data Cliente = OrgGov String
             | Empresa String Integer String String
             | Individuo Pessoa Bool
             deriving Show

data Pessoa = Pessoa String String Genero
            deriving Show 

data Genero = Masculino | Feminino | Outro 
            deriving Show 

primeiro :: [Int] -> Int 
primeiro (x:xs) = x

cauda :: [Int] -> [Int]
cauda (x:xs) = xs

fst3 ::(Int,Int,Int) -> Int 

-- -- data Nat = Zero 
--             | Suc Nat 
-- um :: Nat 
-- um = Suc zero 
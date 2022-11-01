module Cliente where 

data Cliente = OrgGov String
             | Empresa String Integer String String
             | Individuo Pessoa Bool
             deriving Show

data Pessoa = Pessoa String String Genero
            deriving Show 

data Genero = Masculino | Feminino | Outro 
            deriving Show 
-- PROGRAMAÇÃO FUNCIONAL 

-- FICHA 6 

-- EXERCÍCIO 1 

-- ALÍNEA a) 

data Btree a = Empty 
             | Node a (Btree a) (Btree a)
             deriving Show 

altura :: Btree a -> Int 
altura Empty = 0 
altura (Node r e d) = 1 + max (altura e) (altura d)

-- ALÍNEA b)

contaNodos :: Btree a -> Int 
contaNodos Empty = 0 
contaNodos (Node r e d) = 1 + contaNodos e + contaNodos d

-- ALÍNEA c) 

folhas :: Btree a -> Int
folhas Empty = 0 
folhas (Node r Empty Empty) = 1 
folhas (Node r e d) = folhas e + folhas d 

-- ALÍNEA d) 

prune :: Int -> Btree a -> Btree a
prune _ Empty = Empty
prune 0 (Node r e d) = Empty
prune x (Node r e d) = (Node r (prune (x-1) e) (prune (x-1) d))

-- ALÍNEA e)

path :: [Bool] -> Btree a -> [a]
path _ Empty = []
path [] (Node r e d) = [r]
path (True:xs) (Node r e d) = r : path xs d
path (False:xs) (Node r e d) = r : path xs e

-- ALÍNEA f)

mirror :: Btree a -> Btree a
mirror Empty = Empty
mirror (Node r e d) = (Node r (mirror d) (mirror e))

-- ALÍNEA g)

zipWithBT :: (a->b->c) -> Btree a -> Btree b -> Btree c
zipWithBT f _ Empty = Empty
zipWithBT f Empty _ = Empty
zipWithBT f (Node r1 e1 d1) (Node r2 e2 d2) = (Node (f r1 r2) e d)
                                        where e = zipWithBT f e1 e2
                                              d = zipWithBT f d1 d2

-- ALÍNEA h)

unzipBT :: Btree (a,b,c) -> (Btree a, Btree b, Btree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (r1,r2,r3) e d) = (Node r1 a x, Node r2 b y, Node r3 c z)
                          where (a,b,c) = unzipBT e
                                (x,y,z) = unzipBT d

-- EXERCÍCIO 2 

-- ALÍNEA a) 

minimo :: Ord a => Btree a -> a
minimo (Node r Empty Empty) = r 
minimo (Node r e d) = minimo e 

-- ALÍNEA b)

semMinimo :: Ord a => Btree a -> Btree a
semMinimo (Node r Empty Empty) = Empty
semMinimo (Node r e d) = (Node r (semMinimo e) d)

-- ALÍNEA c) 

minSmin :: Ord a => Btree a -> (a,Btree a)
minSmin (Node r Empty Empty) = (r, Empty)
minSmin (Node r e d) = (minimo e, Node r (semMinimo e) d)

-- ALÍNEA d) 

remove :: Ord a => a -> Btree a -> Btree a 
remove _ Empty = Empty
remove x (Node r e d) | x == r = removeAux (Node r e d)
                      | x < r = (Node r (remove x e) d)
                      | x > r = (Node r e (remove x d))

removeAux :: Btree a -> Btree a
removeAux (Node r Empty d) = d
removeAux (Node r e Empty) = e
removeAux (Node r e d) = let m = maior e
                             e' = semMaior e
                         in (Node m e' d)

maior (Node r _ Empty) = r 
maior (Node _ _ d) = maior d 
semMaior (Node r e Empty) = e 
semMaior (Node r e d) = (Node r e (semMaior d))

-- EXERCÍCIO 3

-- ALÍNEA a)

type Aluno = (Numero,Nome,Regime,Classificacao)

type Numero = Int

type Nome = String 

data Regime = ORD | TE | MEL
            deriving Show

data Classificacao = Aprov Int
                   | Rep 
                   | Faltou 
                   deriving Show

type Turma = Btree Aluno

inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False 
inscNum x (Node (n,no,r,c) e d) | x == n = True 
                                | x < n = inscNum x e 
                                | otherwise = inscNum x d

-- ALÍNEA b)

inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome x (Node (n,no,r,c) e d) | x == no = True 
                                 | otherwise = inscNome x e || inscNome x d 

-- ALÍNEA c) 

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = [] 
trabEst (Node (n,no,TE,c) e d) = (n,no) : (trabEst e ++ trabEst d)
trabEst (Node (n,no,r,c) e d) = (trabEst e ++ trabEst d)

-- ALÍNEA d) 

nota :: Numero -> Turma -> Maybe Classificacao
nota _ Empty = Nothing
nota x (Node (n,no,r,c) e d) | x == n = Just c 
                             | x < n = nota x e 
                             | otherwise = nota x d 

-- ALÍNEA e) 

percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas t = (faltas t) / fromIntegral (contaNodos t)

faltas :: Turma -> Float
faltas Empty = 0
faltas (Node (x,y,z,Faltou) e d) = 1 + faltas e + faltas d
faltas (Node (x,y,z,w) e d) = faltas e + faltas d

-- ALÍNEA f) 

mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov t = (somaAprov t) / (aprov t)

aprov :: Turma -> Float
aprov Empty = 0
aprov (Node (x,y,z,Aprov n) e d) = 1 + aprov e + aprov d
aprov (Node (x,y,z,w) e d) = aprov e + aprov d

somaAprov :: Turma -> Float
somaAprov Empty = 0
somaAprov (Node (x,y,z,Aprov n) e d) = (fromIntegral n) + somaAprov e + somaAprov d
somaAprov (Node (x,y,z,w) e d) = somaAprov e + somaAprov d

-- ALÍNEA g)

aprovAv :: Turma -> Float
aprovAv Empty = 0
aprovAv turma = (fromIntegral (aux1 turma)) / fromIntegral (contaNodos turma)

aux1 :: Turma ->  Int
aux1 Empty = 0
aux1 (Node (x,y,z,Aprov n) e d) = 1 + aux1 e + aux1 d
aux1 (Node (x,y,z,w) e d) = aux1 e + aux1 d
-- PROGRAMAÇÃO FUNCIONAL 

-- FICHA 2 

import Data.Char 

-- EXERCÍCIO 1 
-- ALÍNEA a)

funA :: [Double] -> Double 
funA [] = 0 
funA (y:ys) = y^2 + (funA ys)

{-- Pega no primeiro elemento da lista e mete-o ao quadrado.
Depois pega na tail da primeira lista e aplica-lhe a função "funA" novamente 
pegando no primeiro elemento dessa lista e metendo-o ao quadrado 
e assim sucessivamente. Neste caso fica: 
    funA [2,3,5,1]
         = [2:[3,5,1]]
    funA [2:[3,5,1]]
= 2^2 + funA [3,5,1]
= 4 + funA [3:[5,1]]
= 4 + 3^2 + funA [5,1] = ... = 39 --}

-- ALÍNEA b) 

funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2) == 0
           then h : (funB t)
           else (funB t)

{-- Pega no primeiro elemento da lista e verifica se este é par.
Depois pega na tail da lista e aplica-lhe a função "funB" novamente 
pegando no primeiro elemento dessa lista e verificando se este é par 
e assim sucessivamente. Neste caso fica:
    funB [8,5,12]
         = [8:[5,12]]
    funB [8:[5,12]]
= 8 : funB [5,12]
= 8 : funB [5:[12]]
= 8 : funB [12]
= 8:12: funB []
= [8,12] --}

-- ALÍNEA c) 

funC (x:y:t) = funC t
funC [x] = []
funC [] = []

{-- Dá-se uma lista e devolve uma lista vazia
    funC [1,2,3,4,5] 
= funC [3,4,5]
= func [5]
= []
    funC [] = [] --}

-- ALÍNEA d)

funD l = g [] l
g l [] = l
g l (h:t) = g (h:l) t 

{-- Pega numa lista de caracteres e inverte-a 
    funD "otrec"
= g [] "otrec"
= g o:[] "trec"
= g t:o:[] "rec"
= g r:t:o:[] "ec"
= g e:r:t:o:[] "c"
= g c:e:r:t:o:[] 
= g "certo" --} 

-- EXERCÍCIO 2 
-- ALÍNEA a)

dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = h*2 : dobros t 

-- ALÍNEA b)

numOcorre :: Char -> String -> Int 
numOcorre c [] = 0 
numOcorre c (h:t) | c == h = 1 + numOcorre c t
                  | otherwise = numOcorre c t 

-- ALÍNEA c) 

positivos :: [Int] -> Bool 
positivos [h] = h > 0 
positivos (h:t) = if h < 0 then False else positivos t 

-- ALÍNEA d)

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if h <= 0 then soPos t else h: soPos t 

-- ALÍNEA e) 

somaNeg :: [Int] -> Int 
somaNeg [x] = if x < 0 then x else 0
somaNeg (h:t) = if h < 0 then h + somaNeg t else somaNeg t 

-- ALÍNEA f)

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt [x] = [x]
tresUlt [x,y] = [x,y]
tresUlt [x,y,z] = [x,y,z]
tresUlt l = tresUlt (tail l)

-- ALÍNEA g)

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((a,b):t) = b : segundos t 

-- ALÍNEA h)

nosPrimeiros :: Eq a => a -> [(a,b)] -> Bool 
nosPrimeiros _ [] = False 
nosPrimeiros x ((a,b):t) | x == a = True 
                         | otherwise = nosPrimeiros x t 

-- ALÍNEA i)

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):t) = let (x,y,z) = sumTriplos t 
                         in (a+x,b+y,c+z)

-- EXERCÍCIO 3 
-- ALÍNEA a)

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) = if h >= '0' && h <= '9' then h : soDigitos t else soDigitos t  

-- ALÍNEA b) 

minusculas :: [Char] -> Int 
minusculas [] = 0 
minusculas (h:t) | h >= 'a' && h <= 'z' = 1 + minusculas t
                 | otherwise = minusculas t 

-- ALÍNEA c) 

nums :: String -> [Int]
nums "" = []
nums (h:t) | h >= '0' && h <= '9' = digitToInt h : nums t 
           | otherwise = nums t 

-- EXERCÍCIO 4 
-- ALÍNEA a)

type Polinomio = [Monomio]
type Monomio = (Float,Int)

conta :: Int -> Polinomio -> Int
conta n [] = 0 
conta n ((a,b):t) | n == b = 1 + conta n t 
                  | otherwise = conta n t 

-- ALÍNEA b)

grau :: Polinomio -> Int 
grau [] = 0 
grau [(a,b)] = b 
grau ((a,b):(c,d):t) | b > d = grau ((a,b):t) 
                     | otherwise = grau ((c,d):t)

-- ALÍNEA c) 

selgrau :: Int -> Polinomio -> Polinomio 
selgrau _ [] = []
selgrau x (h:t) | x == snd h = h : selgrau x t 
                | otherwise = selgrau x t 

-- ALÍNEA d)

deriv :: Polinomio -> Polinomio
deriv [] = [] 
deriv ((a,b):t) = (a*(fromIntegral b), b-1) : deriv t 

-- ALÍNEA e)

calcula :: Float -> Polinomio -> Float 
calcula x [] = 0 
calcula x ((a,b):t) = a * (x^b) + calcula x t  

-- ALÍNEA f) 

simp :: Polinomio -> Polinomio
simp [] = [] 
simp ((a,b):t) | b == 0 = simp t 
               | otherwise = (a,b) : simp t 

-- ALÍNEA g)

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (x,y) ((a,b):t) = (x*a, y+b) : (mult (x,y) t)

-- ALÍNEA h)

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza (h:ts) = acresAux h (normaliza ts) where
    acresAux :: Monomio -> Polinomio -> Polinomio
    acresAux (c,e) [] = [(c,e)]
    acresAux (c,e) ((x,y):ts) = if (e == y) then (c + x, e):ts else (x,y) : (acresAux (c,e) ts)

-- ALÍNEA i)

soma :: Polinomio -> Polinomio -> Polinomio
soma p [] = p
soma p (h:t) = soma (somaaux h p) t

somaaux :: Monomio -> Polinomio -> Polinomio
somaaux (x,y) [] = []
somaaux (x,y) ((a,b):t) | y < b  = (x,y):(a,b):t
                        | y == b = (a+x,b):t
                        | y > b  = (a,b):somaaux (x,y) t
-- ALÍNEA j)

produto :: Polinomio -> Polinomio -> Polinomio
produto p [] = p
produto p (h:t) = produto (prodaux h p) t

prodaux :: Monomio -> Polinomio -> Polinomio
prodaux (x,y) [] = []
prodaux (x,y) ((a,b):t) = (x*a,y+b) : prodaux (x,y) t


-- ALÍNEA k) 

ordenap :: Polinomio -> Polinomio
ordenap [] = []
ordenap (h:t) = ordenapaux h (ordenap t)

ordenapaux :: Monomio -> Polinomio -> Polinomio
ordenapaux (x,y) [] = [(x,y)]
ordenapaux (x,y) ((a,b):t) = if (y<=b) then (x,y):(a,b):t
                                       else (a,b):ordenapaux (x,y) t

-- ALÍNEA l)

equiv :: Polinomio -> Polinomio -> Bool
equiv p [] = False
equiv p [(x,y)] = equivaux (x,y) p
equiv p (h:t) = (equivaux h p) && (equiv p t)

equivaux :: Monomio -> Polinomio -> Bool
equivaux (x,y) [] = False
equivaux (x,y) ((a,b):t) = if (x==a && y==b) then True else equivaux (x,y) t
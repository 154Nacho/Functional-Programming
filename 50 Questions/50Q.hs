-- PROGRAMAÇÃO FUNCIONAL

-- 50 QUESTÕES

--1 enumFromTo

myenumFromTo :: Int -> Int -> [Int]
myenumFromTo a b | (a>b) = []
                 | otherwise = a : myenumFromTo (a+1) b

--2 enumFromThemTo

myenumFromThenTo :: Int -> Int -> Int -> [Int]
myenumFromThenTo a b c | a<b && a>c = []
                       | a>b && a<c = []
                       | a==c = [a]
                       | otherwise = a : myenumFromThenTo b (b + (b-a)) c

--3 (++)

myplusplus :: [a] -> [a] -> [a]
myplusplus [] l = l
myplusplus l [] = l
myplusplus (h : t) l = h : myplusplus t l

--4 (!!)

myindex :: [a] -> Int -> a
myindex (h : t) 0 = h
myindex (h : t) a = myindex t (a-1)

--5 reverse

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (h : t) = myreverse t ++ [h]

--6 take

mytake :: Int -> [a] -> [a]
mytake 0 _ = []
mytake x [] = []
mytake a (h : t) = h : mytake (a-1) t

--7 drop

mydrop :: Int -> [a] -> [a]
mydrop 0 l = l
mydrop x [] = []
mydrop a (h : t) = mydrop (a-1) t

--8 zip

myzip :: [a] -> [a] -> [(a,a)]
myzip [] [] = []
myzip l [] = []
myzip [] l = []
myzip (h : t) (i : j) = (h,i) : myzip t j

--9 elem

myelem :: Eq a => a -> [a] -> Bool
myelem x [] = False
myelem a (h : t) | a==h = True
                 | otherwise = myelem a t

--10 replicate

myreplicate :: Int -> a -> [a]
myreplicate 0 a = []
myreplicate x a = a : myreplicate (x-1) a

--11 interspace 

myinterspace :: a -> [a] -> [a]
myinterspace a [] = []
myinterspace a [x] = [x]
myinterspace a (h : t) = h : a : myinterspace a t 

--12 group

mygroup :: Eq a => [a] -> [[a]]
mygroup [] = []
mygroup (h : t) = (aux9 h t) : mygroup(drop(length(aux9 h t))(h : t))

aux9 :: Eq a => a -> [a] -> [a]
aux9 a [] = [a]
aux9 a (i : j) | (a==i) = i : aux9 a j
               | otherwise = aux9 a []

--13 concat

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (h : t) = h ++ myconcat t

--14 inits 

inits :: [a] -> [[a]]
inits [x] = [[],[x]]
inits l = inits (myinit l) ++ [l]

myinit :: [a] -> [a]
myinit [] = []
myinit [x] = []
myinit (h : t) = h : myinit t

--15 tails

tails :: [a] -> [[a]]
tails [a] = [[a],[]]
tails l = [l] ++ tails (mytail l) 

mytail :: [a] -> [a]
mytail [] = []
mytail (h : t) = t

--16 isPrefixOf

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (h : t) (i : j) | h==i = isPrefixOf t j
                           | otherwise = False

--17 isSufixOf

isSufixOf :: Eq a => [a] -> [a] -> Bool
isSufixOf [] [] = True
isSufixOf [] _ = False
isSufixOf _ [] = False
isSufixOf l1 l2 = isPrefixOf (reverse l1) (reverse l2)

--18 isSubsequenceOf

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [x] [] = False
isSubsequenceOf [] [x] = True
isSubsequenceOf (h : t) (i : j) | h==i = isSubsequenceOf t j
                                | otherwise = isSubsequenceOf (h : t) j

--19 elemIndices

myelemIndices :: Eq a => a -> [a] -> [Int]
myelemIndices _ [] = []
myelemIndices a l = aux3 0 a l

aux3 :: Eq a => Int -> a -> [a] -> [Int]
aux3 _ _ [] = []
aux3 x a (h : t) | (a == h) = x : aux3 (x+1) a t
                 | otherwise = aux3 (x+1) a t

--20 nub

mynub :: Eq a => [a] -> [a]
mynub [] = []
mynub (h : t) = h : mynub (aux4 h t)

aux4 :: Eq a => a -> [a] -> [a]
aux4 _ [] = []
aux4 a (i : j) | (a==i) = aux4 a j
               | otherwise = i : aux4 a j

--21 delete

mydelete :: Eq a => a -> [a] -> [a]
mydelete a [] = []
mydelete a (h : t) | a==h = t
                   | otherwise = h : mydelete a t

--22 (\\)

myslashslash :: Eq a => [a] -> [a] -> [a]
myslashslash l [] = l
myslashslash [] l = []
myslashslash l (h : t) = myslashslash (mydelete h l) t

--23 union

myunion :: Eq a => [a] -> [a] -> [a]
myunion l [] = l
myunion l (h:t) | elem h t == True = myunion l t
                | otherwise = myunion (l ++ [h]) t  

--24 intersect

myintersect :: Eq a => [a] -> [a] -> [a] 
myintersect [] [] = []
myintersect [] l = []
myintersect (h : t) l | (aux5 h l == True) = h : myintersect t l
                      | otherwise = myintersect t l

aux5 :: Eq a => a -> [a] -> Bool
aux5 a [] = False
aux5 a (i : j) | a==i = True
               | otherwise = aux5 a j

--25 insert

myinsert :: Ord a => a -> [a] -> [a]
myinsert a [] = [a]
myinsert a (h : t) | a>=h = h : myinsert a t
                   | otherwise = a : (h : t)

--26 unwords

myunwords :: [String] -> String
myunwords [x] = x
myunwords [] = []
myunwords (h : t) = h ++ " " ++ myunwords t

--27 unlines

myunlines :: [String] -> String
myunlines [x] = x  ++ "\n"
myunlines [] = []
myunlines (h : t) = h ++ "\n" ++ myunlines t

--28 pMaior

pMaior :: Ord a => [a] -> Int
pMaior (h : t) = aux7 0 (aux6 h t) (h : t)

aux6 :: Ord a => a -> [a] -> a
aux6 x [] = x
aux6 a (i : j) | a>=i = aux6 a j
               | otherwise = aux6 i j

aux7 :: Ord a => Int -> a -> [a] -> Int
aux7 x a (h : t) | (a==h) = x
                 | otherwise = aux7 (x+1) a t 

--29 temRepetidos

temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos [a] = False
temRepetidos (h : t) | (aux8 h t == True) = True
                     | otherwise = temRepetidos t

aux8 :: Eq a => a -> [a] -> Bool
aux8 a [] = False
aux8 a (i : j) | a==i = True
               | otherwise = aux8 a j

--30 algarismos	

myalgarismos :: [Char] -> [Char]
myalgarismos [] = []
myalgarismos (h : t) | h>='0' && h<='9' = h : myalgarismos t
                     | otherwise = myalgarismos t

--31 posImpares

myposImpares :: [a] -> [a]
myposImpares [] = []
myposImpares [x] = []
myposImpares (h:i:t) = i : myposImpares t

--32 posPares

myposPares :: [a] -> [a]
myposPares [] = []
myposPares [a] = [a]
myposPares (h:i:t) = h : myposPares t

--33 isSorted

myisSorted :: Ord a => [a] -> Bool
myisSorted [] = True
myisSorted [x] = True
myisSorted (h:i:t) | h<=i = myisSorted (i:t)
                   | otherwise = False

--34 iSort

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h : t) | (aux10 h t == True) = h : iSort t
              | otherwise = myinsert h (iSort t)

aux10 :: Ord a => a -> [a] -> Bool
aux10 _ [] = True
aux10 x (i : j) | x<=i = aux10 x j
                | otherwise = False

--35 menor 

menor :: String -> String -> Bool
menor "" "" = False
menor "" l = True
menor l "" = False
menor (h:t) (i:j) | h<i = True
                  | h>i = False
                  | h==i = menor t j

--36 elemMset

elemMset :: Eq a => a -> [(a,Int)] -> Bool
elemMset a [] = False
elemMset a ((x,y) : t) | a==x = True
                       | otherwise = elemMset a t

--37 lengthMset

lengthMset :: [(a,Int)] -> Int
lengthMset [] = 0
lengthMset ((x,y) : t) = y + lengthMset t

--38 converteMset

converteMset :: [(a,Int)] -> [a]
converteMset [] = []
converteMset ((x,y) : t) = aux2 y x ++ converteMset t

aux2 :: Int -> a -> [a]
aux2 0 x = []
aux2 y x = [x] ++ aux2 (y - 1) x

--39 insereMset 

insereMset :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMset a [] = [(a,1)]
insereMset a ((x,y) : t) | (a==x) = ((x,y+1) : t)
                         | otherwise = (x,y) : insereMset a t

--40 removeMset

removeMset :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMset a [] = []
removeMset a ((x,y) : t) | (a==x) && (y>1) = ((x,y-1) : t)
                         | (a==x) && (y==1) = t
                         | otherwise = (x,y) : removeMset a t

--41 constroiMset

constroiMset :: Ord a => [a] -> [(a,Int)]
constroiMset [] = []
constroiMset (h : t) = (h,aux11 h t 1) : constroiMset(aux12 h t)

aux11 :: Ord a => a -> [a] -> Int -> Int
aux11 h [] x = x
aux11 h (i : j) x | h==i = aux11 h j (x+1)
                  | otherwise = aux11 h j x

aux12 :: Ord a => a -> [a] -> [a]
aux12 _ [] = []
aux12 y (x : xs) | y==x = aux12 y xs
                 | otherwise = x : aux12 y xs

--42 partitionEithers

partEithers :: [Either a b] -> ([a],[b])
partEithers [] = ([],[])
partEithers l = (lefts l, rights l)

lefts :: [Either a b] -> [a] 
lefts [] = []
lefts (Left a : t) = a : lefts t
lefts (Right b : t) = lefts t

rights :: [Either a b] -> [b]
rights [] = []
rights (Right b : t) = b : rights t
rights (Left a : t) = rights t

--43 catMaybes

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just a : t) = a : catMaybes t
catMaybes (Nothing : t) = catMaybes t

--44 posicao

data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (h : t) | (isNorte h) = posicao (x,y+1) t
                      | (isSul h) = posicao (x,y-1) t
                      | (isEste h) = posicao (x+1,y) t
                      | (isOeste h) = posicao (x-1,y) t

isNorte Norte = True
isNorte _ = False

isSul Sul = True
isSul _ = False

isEste Este = True
isEste _ = False

isOeste Oeste = True
isOeste _ = False


--45 caminho

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (a,b) | y<b = Norte : caminho (x,y+1) (a,b)
                    | y>b = Sul : caminho (x,y-1) (a,b)
                    | x<a = Este : caminho (x+1,y) (a,b)
                    | x>a = Oeste : caminho (x-1,y) (a,b)
                    | (x==a && y==b) = []

--46 vertical

vertical :: [Movimento] -> Bool
vertical [] = True
vertical (h:t) | (isNorte h) || (isSul h) = vertical t
               | otherwise = False 

{- 
isNorte Norte = True
isNorte _ = False
isSul Sul = True
isSul _ = False
-}

--47 maisCentral 

data Posicao = Pos Int Int deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [x] = x
maisCentral (h : t) = aux13 h t

aux13 :: Posicao -> [Posicao] -> Posicao
aux13 h [] = h
aux13 h (i : j) | (aux14 h < aux14 i) = aux13 h j
                | otherwise = aux13 i j

aux14 :: Posicao -> Int
aux14 (Pos a b) = (abs(a)+abs(b))

--48 vizinhos

vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos x (h : t) | ((aux17 x h) || (aux18 x h) == True) = h : vizinhos x t
                   | otherwise = vizinhos x t

aux17 :: Posicao -> Posicao -> Bool
aux17 (Pos a b) (Pos x y) | (a==(x-1) && b==y) || (a==(x+1) && b==y) = True
                          | otherwise = False

aux18 :: Posicao -> Posicao -> Bool
aux18 (Pos a b) (Pos x y) | (b==(y-1) && a==x) || (b==(y+1) && a==x) = True
                          | otherwise = False

--49 mesmaOrdenada

mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = False
mesmaOrdenada (h : t) = aux15 h t

aux15 :: Posicao -> [Posicao] -> Bool
aux15 _ [] = True 
aux15 (Pos a b) ((Pos x y) : j) | b==y = aux15 (Pos a b) j
                                | otherwise = False

--50 intercecaoOK

data Semaforo = Verde | Amarelo | Vermelho deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK [] = True
interseccaoOK (Vermelho : t) = interseccaoOK t
interseccaoOK (_ : t) = aux16 t

aux16 :: [Semaforo] -> Bool
aux16 [] = True
aux16 (Vermelho : t) = aux16 t
aux16 (_ : t) = False
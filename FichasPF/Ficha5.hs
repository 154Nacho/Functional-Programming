-- PROGRAMAÇÃO FUNCIONAL 

-- FICHA 5

-- EXERCÍCIO 1

-- ALÍNEA a)

myAny :: (a->Bool) -> [a] -> Bool 
myAny _ [] = False 
myAny f (h:t) = if f h then True else myAny f t

-- ALÍNEA b) 

myZipWith :: (a->b->c) -> [a] -> [b] -> [c]
myZipWith f _ [] = []
myZipWith f [] _ = []
myZipWith f (x:xs) (y:ys) = (f x y) : myZipWith f xs ys 

-- ALÍNEA c)

myTakeWhile :: (a->Bool) -> [a] -> [a]
myTakeWhile f [] = [] 
myTakeWhile f (h:t) = if f h then h : myTakeWhile f t else [] 

-- ALÍNEA d) 

myDropWhile :: (a->Bool) -> [a] -> [a]
myDropWhile f [] = []
myDropWhile f (h:t) = if f h then myDropWhile f t else (h:t) 

-- ALÍNEA e) 

mySpan :: (a->Bool) -> [a] -> ([a],[a])
mySpan f (h:t) = aux f (h:t) [] [] 
               where aux f [] y1 y2 = (y1,y2)
                     aux f (h:t) y1 y2 = if f h then aux f t (y1 ++ [h]) y2 else aux f [] y1 (h:t)

-- ALÍNEA f) 

myDeleteBy :: (a->a->Bool) -> a -> [a] -> [a]
myDeleteBy f _ [] = []
myDeleteBy f x (h:t) = if (f x h) then t else h: myDeleteBy f x t

-- ALÍNEA g)

mySortOn :: Ord b => (a->b) -> [a] -> [a]
mySortOn f [] = [] 
mySortOn f (h:t) = insert f h (mySortOn f t)

insert :: Ord b => (a->b) -> a -> [a] -> [a]
insert f x [] = [x]
insert f x (h:t) | f x > f h = h : (insert f x t)
                 | otherwise = x:h:t 

-- EXERCÍCIO 2 

-- ALÍNEA a) 

type Polinomio = [Monomio]

type Monomio = (Float, Int) 

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = [] 
selgrau p l = filter f l 
            where f (c,e) = p == e 

-- ALÍNEA b)

conta :: Int -> Polinomio -> Int 
conta n l = length (selgrau n l)

-- ALÍNEA c) 

grau :: Polinomio -> Int 
grau [] = 0 
grau (h:t) = checkG h t 

checkG :: Monomio -> Polinomio -> Int 
checkG (_,e) [] = e 
checkG (c,e) ((c1,e1):t) | e > e1 = checkG (c,e) t 
                         | otherwise = checkG (c1,e1) t 

-- ALÍNEA d) 

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv l = map (\(x,y) -> (x*(realToFrac y),y-1)) l 

-- ALÍNEA e) 

calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula x t = sum (map (\(c,e) -> (x*c)^e) t)

-- ALÍNEA f)

simp :: Polinomio -> Polinomio
simp [] = [] 
simp l = filter f l 
       where f (c,e) = e /= 0 

-- ALÍNEA g) 

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = [] 
mult (a,b) l = map ((\(c,e) -> (c*a,e*b))) l 

-- ALÍNEA h) 

ordena :: Polinomio -> Polinomio
ordena p = mySortOn (\(a,b) -> b) p  

-- ALÍNEA i)

normaliza :: Polinomio -> Polinomio 
normaliza [] = []
normaliza p@((a,b):t) = let l = filter (\(x,y) -> y == b) p
                            d = filter (\(x,y) -> y /= b) p
                            w = sum (map (fst) l)
                            in (w,b) : normaliza d

-- ALÍNEA j)

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = myZipWith (\(a,b) (x,y) -> (a+x,b)) p1 p2 

-- ALÍNEA k)

produto :: Polinomio -> Polinomio -> Polinomio
produto p [] = []
produto p1 p2 = mult (head p1) p2 ++ produto (tail p1) p2

-- ALÍNEA l) 

equiv :: Polinomio -> Polinomio -> Bool 
equiv [] [] = True
equiv p1 p2 | a == b = equiv d c
            | otherwise = False  
            where (a:d) = ordena (normaliza p1)
                  (b:c) = ordena (normaliza p2)

-- EXERCÍCIO 3

-- ALÍNEA a) 

type Mat a = [[a]]

dimOK :: Mat a -> Bool 
dimOK [] = True 
dimOK (x:y:t) | length x == length y = dimOK (y:t)
              | otherwise = False 

-- ALÍNEA b) 

dimMat :: Mat a -> (Int,Int)
dimMat [[]] = (0,0)
dimMat m = (length m , length (head m)) 

-- ALÍNEA c) 

addMat :: Num a => Mat a -> Mat a -> Mat a 
addMat m [] = m 
addMat [] m = m
addMat (h1:t1) (h2:t2) = (myZipWith (+) h1 h2) : (addMat t1 t2)

-- ALÍNEA d) 

transpose :: Mat a -> Mat a
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

-- ALÍNEA e)

multMat :: Num a => Mat a -> Mat a -> Mat a 
multMat [] _ = [] 
multMat _ [] = [] 
multMat (h1:t1) (h2:t2) = (myZipWith (*) h1 h2) : (multMat t1 t2)

-- ALÍNEA f) 

zipWMat :: (a->b->c) -> Mat a -> Mat b -> Mat c 
zipWMat = undefined

-- ALÍNEA g) 

triSup :: Num a => Mat a -> Bool 
triSup = undefined

-- ALÍNEA h) 

rotateLeft :: Mat a -> Mat a
rotateLeft [] = []
rotateLeft m = rotateLeft (tail m) ++ [head (transpose m)] 
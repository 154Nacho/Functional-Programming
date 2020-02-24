-- PROGRAMAÇÃO FUNCIONAL 

-- FICHA 1 

import Data.Char 

-- EXERCÍCIO 1 
-- ALÍNEA a) 

perimetro :: (Float) -> Float 
perimetro r = 2*pi*r

-- ALÍNEA b) 

dist :: (Double, Double) -> (Double, Double) -> Double 
dist (x1,y1) (x2,y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

-- ALÍNEA c)

primUlt :: [a] -> (a,a)
primUlt l = (head l, last l)

-- ALÍNEA d) 

multiplo :: Int -> Int -> Bool 
multiplo m n = (mod m n) == 0

-- ALÍNEA e) 

truncaImpar :: [a] -> [a]
truncaImpar l | mod (length l) 2 == 0 = l 
              | otherwise = tail l

-- ALÍNEA f) 

max2 :: Int -> Int -> Int 
max2 x y | x > y = x
         | otherwise = y 

-- AlÍNEA g) 

max3 :: Int -> Int -> Int -> Int 
max3 x y z = max2 z (max2 y z)

-- EXERCÍCIO 2
-- ALÍNEA a)

nRaizes :: (Double, Double, Double) -> Int  
nRaizes (a,b,c) | b^2 - 4*a*c == 0 = 1 
                | b^2 - 4*a*c > 0 = 2
                | otherwise = 0 

-- ALÍNEA b) 

raizes :: (Double, Double, Double) -> [Double]
raizes (a,b,c) | nRaizes (a,b,c) == 2 = [((-b) + sqrt (b^2-4*a*c)/2*a), ((-b) - sqrt (b^2-4*a*c)/2*a)]
               | nRaizes (a,b,c) == 1 = [(-b)/2*a]
               | otherwise = []

-- EXERCÍCIO 3
-- ALÍNEA a) 

type Hora = (Int, Int)

horaValida :: (Int, Int) -> Bool 
horaValida (h,m) | (h>=0 && h<=24) && (m>=0 && m<=60) = True 
                 | otherwise = False

-- ALÍNEA b)

comparaHora :: (Int,Int) -> (Int, Int) -> Bool 
comparaHora (h1,m1) (h2,m2) | h1 > h2 = True
                            | h1 == h2 && m1 > m2 = True
                            | otherwise = False


-- ALÍNEA c) 

converteHoraEmMinuto :: Hora -> Int
converteHoraEmMinuto (h,m) = (h*60) + m 

-- ALÍNEA d) 

converteMinutoEmHora :: Int -> Hora
converteMinutoEmHora m = (div m 60, mod m 60)

-- ALÍNEA e) 

diferencaHora :: Hora -> Hora -> Int 
diferencaHora (h1,m1) (h2,m2) = converteHoraEmMinuto (h1,m1) - converteHoraEmMinuto (h2,m2)

-- ALÍNEA f) 

addMinutos :: Hora -> Int -> Hora 
addMinutos (h1,m1) m = converteMinutoEmHora (converteHoraEmMinuto (h1,m1) + m)

-- EXERCÍCIO 4
-- ALÍNEA a) 

data Horas = H Int Int deriving (Show,Eq)

horaValida' :: Horas -> Bool
horaValida' (H h m) = (h >= 0 && h < 24 && m >= 0 && m < 60) 

-- ALÍNEA b)

comparaHora' :: Horas -> Horas -> Bool 
comparaHora' (H h1 m1) (H h2 m2) | h1 > h2 = True 
                                 | h1 == h2 && m1 > m2 = True
                                 | otherwise = False

-- ALÍNEA c) 

converteHoraEmMinuto' :: Horas -> Int 
converteHoraEmMinuto' (H h m) = (h*60) + m 

-- ALÍNEA d) 

converteMinutoEmHora' :: Int -> Horas
converteMinutoEmHora' m = H (div m 60) (mod m 60)

-- ALÍNEA e) 

diferencaHora' :: Horas -> Horas -> Int 
diferencaHora' (H h1 m1) (H h2 m2) = abs (converteHoraEmMinuto' (H h1 m1) - converteHoraEmMinuto' (H h2 m2))

-- ALÍENA f) 

addMinutos' :: Int -> Horas -> Horas
addMinutos' m2 (H h1 m1) = H (div (m1+m2) 60) (mod (m1+m2) 60)  

-- EXERCÍCIO 5
-- ALÍNEA a) 

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

next :: Semaforo -> Semaforo
next d | d == Verde = Amarelo
       | d == Amarelo = Vermelho 
       | otherwise = Verde 

-- ALÍNEA b) 

stop :: Semaforo -> Bool 
stop d = (d == Vermelho)

-- ALÍNEA c)

safe :: Semaforo -> Semaforo -> Bool 
safe d1 d2 | d1 == Vermelho && d2 == Verde = True 
           | d1 == Verde && d2 == Verde = True
           | otherwise = False 

-- EXERCÍCIO 6

-- ALÍNEA a) 

data Ponto = Cartesiano Double Double | Polar Double Double
             deriving (Show,Eq)

posx :: Ponto -> Double 
posx (Cartesiano x y) = abs x 
posx (Polar r t) = abs (r*cos t)

-- ALÍNEA b) 

posy :: Ponto -> Double
posy (Cartesiano x y) = abs y 
posy (Polar r t) = abs (r* sin t)

-- ALÍNEA c)

raio :: Ponto -> Double 
raio (Cartesiano x y) = sqrt (x^2 + y^2) 
raio (Polar r t) = r 

-- ALÍNEA d) 

angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan (y/x)
angulo (Polar r t) = t

-- ALÍENA e)

distancia :: Ponto -> Ponto -> Double 
distancia (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)
distancia (Polar x1 y1) (Polar x2 y2) | (x1 /= 0 && x2 /= 0) = sqrt (x1^2 + x2^2 - 2*x1*x2*cos(y1-y2))
                                 | (x1 == 0 && x2 /= 0) = sqrt (x2^2 - 2*x2*cos(y1-y2))
                                 | (x1 /= 0 && x2 == 0) = sqrt (x1^2 - 2*x1*cos(y1-y2))
                                 | otherwise = sqrt (2*cos(y1-y2))

-- EXERCÍCIO 8 
-- ALÍNEA a) 

myIsLower :: Char -> Bool 
myIsLower c = ord c >= ord 'a' && ord c <= ord 'z' 

-- ALÍNEA b)

myIsDigit :: Char -> Bool 
myIsDigit c = ord c >= ord '0' && ord c <= ord '9' 

-- ALÍNEA c) 

myIsAlpha :: Char -> Bool 
myIsAlpha c = ord c >= ord 'A' && ord c <= ord 'z'

-- ALÍNEA d) 

myToUpper :: Char -> Char 
myToUpper c = chr ((ord c) - 32)

-- ALÍNEA e)

myIntToDigit :: Int -> Char 
myIntToDigit x = chr x 

-- ALÍNEA f) 

myDigitToInt :: Char -> Int 
myDigitToInt c = ord c
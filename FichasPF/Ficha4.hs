-- PROGRAMAÇÃO FUNCIONAL 

-- FICHA 4 

import Data.Char

-- EXERCÍCIO 3 

digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha l = (filter isDigit l, filter isAlpha l)

-- EXERCÍCIO 4

nzp :: [Int] -> (Int, Int, Int)
nzp [] = (0,0,0)
nzp l = (length (filter (<0) l), length (filter (==0) l), length (filter (>0) l))

-- EXERCÍCIO 5 

divMod :: Integral a => a -> a -> (a,a)
divMod = undefined 

-- EXERCÍCIO 6

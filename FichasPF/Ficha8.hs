-- PROGRAMAÇÃO FUNCIONAL 

-- FICHA 8 

-- EXERCÍCIO 1 

-- ALÍNEA a)

data Frac = F Integer Integer

normaliza :: Frac -> Frac 
normaliza (F n d) = F x1 y1 
          where m = mdc n d
                x1 = s * (div (abs n) m)
                y1 = (div (abs d) m)
                s = if n*d > 0 then 1 else (-1) 

mdc :: Integer -> Integer -> Integer
mdc x y | x < y = mdc (y-x) x  
        | x > y = mdc (x-y) y
        | x == y = x 

-- ALÍNEA b)

instance Eq Frac where
 (F x y) == (F w z) = ((x*z) == (y*w))

-- ALÍNEA c) 

instance Ord Frac where
 compare (F x y) (F x1 y1) = compare (fromIntegral x/fromIntegral y) (fromIntegral x1/fromIntegral y1)

-- ALÍNEA d)

instance Show Frac where
 show (F x y) = show x ++ "/" ++ show y 

-- ALÍNEA e)

instance Num Frac where
 (F x y) + (F x1 y1) = normaliza (F (x*y1 + x1*y) (y*y1))
 (F x y) - (F x1 y1) = normaliza (F (x*y1 - x1*y) (y*y1))
 (F x y) * (F x1 y1) = normaliza (F (x*x1) (y*y1))
 negate (F x y) = F (-x) y
 abs (F x y) = F (abs x) (abs y)
 signum (F x y) = F (signum x * signum y) 1
 fromInteger n = F n 1

-- EXERCÍCIO 2

-- ALÍNEA a)

data Exp a = Const a | Simetrico (Exp a) | Mais (Exp a) (Exp a) | Menos (Exp a) (Exp a) | Mult (Exp a) (Exp a)

instance Show a => Show (Exp a)  where
 show (Const a) = show a 
 show (Simetrico e) = "-" ++ show e
 show (Mais x y) = show x ++ "+" ++ show y
 show (Menos x y) = show x ++ "-" ++ show y
 show (Mult x y) = show x ++ "*" ++ show y 

-- ALÍNEA b)

instance (Num a,Eq a) => Eq (Exp a) where
 x == y = calcula x == calcula y

calcula :: Num a => Exp a -> a 
calcula (Const a) = a 
calcula (Simetrico x) = - (calcula x)
calcula (Mais x y) = (calcula x) + (calcula y)
calcula (Menos x y) = (calcula x) - (calcula y)
calcula (Mult x y) = (calcula x) * (calcula y) 

-- ALÍNEA c)

instance Num a => Num (Exp a) where
 x + y = Mais x y 
 x - y = Menos x y 
 x * y = Mult x y 
 negate x = Simetrico x 
 abs x = signum x 
 signum x = Const (signum (calcula x))
 fromInteger x = Const (fromInteger x)

-- EXERCÍCIO 3

-- ALÍNEA a)

data Movimento = Credito Float | Debito Float
data Data = D Int Int Int deriving Eq
data Extracto = Ext Float [(Data, String, Movimento)]

-- ALÍNEA a)

instance Ord Data where
 compare (D d m a) (D d1 m1 a1) = compare (d,m,a) (d1,m1,a1)

-- ALÍNEA b)

instance Show Data where
 show (D d m a) = show d ++ "/" ++ show m ++ "/" ++ show a 

-- ALÍNEA c)

ordena :: Extracto -> Extracto
ordena (Ext x []) = Ext x []
ordena (Ext x l) = (Ext x (ordenaDatas l))

ordenaDatas :: [(Data, String, Movimento)] -> [(Data, String, Movimento)]
ordenaDatas [] = []
ordenaDatas (h:t) = insereData h (ordenaDatas t)

insereData :: (Data, String, Movimento) -> [(Data, String, Movimento)] -> [(Data, String, Movimento)]
insereData (d,s,m) [] = [(d,s,m)]
insereData (d,s,m) ((d1,s1,m1):t) | d <= d1 = (d,s,m) : (d1,s1,m1) : t 
                                  | otherwise = (d1,s1,m1) : (d,s,m) : t 
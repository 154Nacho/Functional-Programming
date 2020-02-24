-- PROGRAMAÇÃO FUNCIONAL 

-- FICHA 7

-- EXERCÍCIO 1 

-- ALÍNEA a) 

data ExpInt = Const ExpInt
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

calcula :: ExpInt -> Int 
calcula (Const x) = calcula x 
calcula (Simetrico x) = - (calcula x)
calcula (Mais x y) = (calcula x) + (calcula y)
calcula (Menos x y) = (calcula x) - (calcula y)
calcula (Mult x y) = (calcula x) * (calcula y)

-- ALÍNEA b) 

infixa :: ExpInt -> String
infixa (Const x) = infixa x
infixa (Simetrico x) = "-" ++ (infixa x)
infixa (Mais x y) = "(" ++ (infixa x) ++ "+" ++ (infixa y) ++ ")"
infixa (Menos x y) = "(" ++ (infixa x) ++ "-" ++ (infixa y) ++ ")"
infixa (Mult x y) = "(" ++ (infixa x) ++ "*" ++ (infixa y) ++ ")"

-- ALÍNEA c)

posfixa :: ExpInt -> String
posfixa (Const x) = posfixa x
posfixa (Simetrico x) = posfixa x ++ " -"
posfixa (Mais x y) = posfixa x ++ " " ++ posfixa y ++ " +"
posfixa (Menos x y) = posfixa x ++ " " ++ posfixa y ++ " -"
posfixa (Mult x y) = posfixa x ++ " " ++ posfixa y ++ " *"

-- EXERCÍCIO 2 

-- ALÍNEA a)

data RTree a = R a [RTree a]

soma :: (Num a) => (RTree a) -> a
soma (R a y) = a + sum (map (soma) y)

-- ALÍNEA b) 

altura :: (RTree a) -> Int
altura (R a []) = 1
altura (R a t) = 1 + maximum (map (altura) t)

-- ALÍNEA c)

prune :: Int -> (RTree a) -> (RTree a)
prune 1 (R a _) = (R a [])
prune alt (R a t) = R a (map (prune (alt-1)) t) 

-- ALÍNEA d)

mirror :: (RTree a) -> (RTree a)
mirror (R a t) = R a (reverse (map mirror t))

-- ALÍNEA e)

postorder :: (RTree a) -> [a]
postorder (R w t) = concat(map postorder t) ++ [w]

-- EXERCÍCIO 3 

-- ALÍNEA a) 

data BTree a = Empty | Node a (BTree a) (BTree a)

data LTree a = Tip a | Fork (LTree a) (LTree a)

ltSum :: Num a => LTree a -> a 
ltSum (Tip a) = a
ltSum (Fork e d) = ltSum e + ltSum d

-- ALÍNEA b) 

listaLT :: (LTree a) -> [a]
listaLT (Tip a) = a : []
listaLT (Fork e d) = listaLT e ++ listaLT d

-- ALÍNEA c)

ltHeight :: LTree a -> Int
ltHeight (Tip a) = 0
ltHeight (Fork e d) = if ltHeight e >= ltHeight d then 1 + ltHeight e
                      else 1 + ltHeight d

-- EXERCÍCIO 4 

-- ALÍNEA a) 

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

splitFTree :: (FTree a b) -> (BTree a, LTree b)
splitFTree tree = let w = aux1 (tree)
                      k = aux2 (tree)
                  in (w,k)

aux1 :: (FTree a b) -> BTree a
aux1 (Leaf b) = Empty
aux1 (No a b c) = Node a (aux1 b) (aux1 c)

aux2 :: (FTree a b) -> LTree b
aux2 (Leaf b) = Tip b
aux2 (No a b c)= Fork (aux2 b) (aux2 c)

-- ALÍNEA b) 

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees = undefined 
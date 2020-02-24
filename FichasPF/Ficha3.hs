-- PROGRAMAÇÃO FUNCIONAL 

-- FICHA 3 

-- EXERCÍCIO 1 
-- ALÍNEA a)

data Hora = H Int Int 
            deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

testarEtapa :: Etapa -> Bool 
testarEtapa (H h1 m1, H h2 m2) = horaValida (H h1 m1) &&
                                 horaValida (H h2 m2) &&
                                 (h2 > h1 || (h1 == h2 && m2 > m1))

horaValida :: Hora -> Bool
horaValida (H h m) = (h >= 0 && h < 24 && m >= 0 && m < 60)

-- ALÍNEA b) 

testarViagem :: Viagem -> Bool 
testarViagem [] = False 
testarViagem ((H h1 m1, H h2 m2):(H h3 m3, H h4 m4):t) | h2 < h3 = testarViagem ((H h3 m3, H h4 m4) :t)
                                                       | otherwise = False 
                                                       
-- ALÍNEA c) 

horaPartidaChegada :: Viagem -> (Hora, Hora)
horaPartidaChegada x = (fst (head x), snd (last x)) 

-- ALÍNEA d)

tempoEfetivo :: Viagem -> Int
tempoEfetivo [] = 0
tempoEfetivo (e:es) = duracaoEtapa e + tempoEfetivo es 
                    where duracaoEtapa (h1,h2) = converteHoraEmMinuto h2 - converteHoraEmMinuto h1

converteHoraEmMinuto :: Hora -> Int
converteHoraEmMinuto (H h m) = (h*60) + m  

converteMinutoEmHora :: Int -> Hora
converteMinutoEmHora m = H (div m 60) (mod m 60)

-- ALÍNEA e)

tempoEspera :: Viagem -> Int
tempoEspera y = let (p,c) = horaPartidaChegada y 
                    totalViagem = converteHoraEmMinuto c - converteHoraEmMinuto p 
                in  totalViagem - (tempoEfetivo y)

-- EXERCÍCIO 2 
--ALÍNEA a)

type Poligonal = [Ponto]

data Ponto = Cartesiano Double Double
             deriving (Show,Eq)

comprimento :: Poligonal -> Double
comprimento [x] = 0
comprimento (h:x:xs) = (distancia h x) + comprimento (x:xs)

distancia :: Ponto -> Ponto -> Double 
distancia (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

-- ALÍNEA b)

eFechada :: Poligonal -> Bool 
eFechada l = head l == last l

-- ALÍNEA c)

data Figura = Quadrado 
            | Triangulo Ponto Ponto Ponto

triangula :: Poligonal -> [Figura]
triangula = undefined

-- ALÍNEA d)

area :: Figura -> Double 
area = undefined

-- ALÍNEA e)

mover :: Poligonal -> Ponto -> Poligonal
mover = undefined 

-- ALÍNEA f)

zoom :: Double -> Poligonal -> Poligonal
zoom = undefined

-- EXERCÍCIO 3
-- ALÍNEA a)

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show

type Nomes = String

type Agenda = [(Nome, [Contacto])]

acrescEmail :: Nomes -> String -> Agenda -> Agenda
acrescEmail n e [] = [(n,[Email e])]
acrescEmail n e ((a,b):t) = if (n == a) then (a,Email e:b):t 
                                        else (a,b) : acrescEmail n e t

-- ALÍNEA b)

verEmails :: Nomes -> Agenda -> Maybe [String]
verEmails n [] = Nothing
verEmails n ((p,[_,_,_,Email s]):t) | n == p = Just [s]
                                    | otherwise = verEmails n t  

-- ALÍNEA c) 

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = [] 
consTelefs ((Casa x):t) = x : (consTelefs t)
consTelefs ((Trab y):t) = y : (consTelefs t)
consTelefs (_:t) = consTelefs t 

-- ALÍNEA d) 

casa :: Nomes -> Agenda -> Maybe Integer 
casa n [] = Nothing
casa n ((p,[Casa x,_,_,_]):t) | n == p = Just x 
                              | otherwise = casa n t 

-- EXERCÍCIO 4 
-- ALÍNEA a)

type Dia = Int

type Mes = Int 

type Ano = Int 

type Nome = String 

data Data = D Dia Mes Ano 
          deriving Show 

type TabDN = [(Nome,Data)]

procura :: Nome -> TabDN -> Maybe Data
procura n [] = Nothing 
procura n ((p,d):t) | n == p = Just d
                    | otherwise = procura n t 

-- ALÍNEA b) 

idade :: Data -> Nome -> TabDN -> Maybe Int 
idade _ _ [] = Nothing 
idade (D d m a) n ((p,da):t) | n == p = Just (a - a1)
                             | otherwise = idade (D d m a) n t 
                             where D d1 m1 a1 = da   

-- ALÍNEA c) 

anterior :: Data -> Data -> Bool 
anterior (D d1 m1 a1) (D d2 m2 a2) | a1 < a2 = True
                                   | a1 == a2 && m1 < m2 = True 
                                   | a1 == a2 && m1 == m2 && d1 < d2 = True 
                                   | otherwise = False 

-- ALÍNEA d) 

ordena :: TabDN -> TabDN
ordena [] = []
ordena ((n,d):t) = auxo (n,d) (ordena t)

auxo :: (Nome,Data) -> TabDN -> TabDN
auxo (x,y) [] = [(x,y)]
auxo (x,d1) ((y,d2):t) = if (anterior d1 d2) then (x,d1):(y,d2):t
                                             else (y,d2): auxo (x,d1) t

-- ALÍNEA e)

porIdade :: Data -> TabDN -> [(Nome,Int)]
porIdade (D d m a) [] = []
porIdade (D d m a) ((x,y):t) = ordena2 (cenas (D d m a) ((x,y):t))

cenas :: Data -> TabDN -> [(Nome,Int)]
cenas _ [] = []
cenas (D d m a) ((x,D d2 m2 a2):t) = if (d==d2 && m==m2) then (x, a - a2):cenas (D d m a) t
                                                         else cenas (D d m a) t

ordena2 :: [(Nome,Int)] -> [(Nome,Int)]
ordena2 [(a,b)] = [(a,b)]
ordena2 ((a,b):t) = auxo2 (a,b) (ordena2 t)

auxo2 :: (Nome,Int) -> [(Nome,Int)] -> [(Nome,Int)]
auxo2 (x,y) [] = [(x,y)]
auxo2 (x,d1) ((y,d2):t) = if (d1 <= d2) then (x,d1):(y,d2):t
                                        else (y,d2): auxo2 (x,d1) t

-- EXERCÍCIO 5
-- ALÍNEA a)

data Movimento = Credito Float | Debito Float 
               deriving Show 

data Datas = Ds Int Int Int 
          deriving Show

data Extrato = Ext Float [(Datas,String,Movimento)]
             deriving Show 

extValor :: Extrato -> Float -> [Movimento]
extValor (Ext x []) n = []
extValor (Ext x ((d,s, Credito c):t)) n | c > n = (Credito c):(extValor (Ext x t) n)
                                        | otherwise = extValor (Ext x t) n
extValor (Ext x ((a,b, Debito c):t)) n  | c > n = (Debito c):(extValor (Ext x t) n)
                                        | otherwise = extValor (Ext x t) n   

-- ALÍNEA b)

filtro :: Extrato -> [String] -> [(Datas,Movimento)]
filtro (Ext x []) _ = [] 
filtro (Ext x ((d,s,m):t)) (st:sts) | st == s = (d,m) : (filtro (Ext x t) sts)
                                    | otherwise = filtro (Ext x t) (st:sts)

-- ALÍNEA c) 

creDeb :: Extrato -> (Float,Float)
creDeb (Ext x []) = (0,0)
creDeb (Ext x t) = (x+(creds t), x-(debs t))

creds :: [(Datas,String,Movimento)] -> Float 
creds ((d,s,Credito y):t) = y + creds t 
creds ((d,s,Debito _):t) = creds t 

debs :: [(Datas,String,Movimento)] -> Float 
debs ((d,s,Debito y):t) = y - debs t
debs ((d,s,Credito _):t) = debs t 

-- ALÍNEA d) 

saldo :: Extrato -> Float 
saldo (Ext x []) = x 
saldo (Ext x ((d,ds,Credito y):t)) = x + y + (saldo (Ext (x+y) t))
saldo (Ext x ((d,ds,Debito y):t)) = x - y + (saldo (Ext (x-y) t))
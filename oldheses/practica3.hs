--SesionLab 3 :: Juan Chozas Sumbera--
--1
data Complejo = Complejo Int Int
  deriving Eq

instance Show Complejo where
  show (Complejo a b) = show a ++ (if b > 0 then "+" else "") ++ (if b /= 0 then (show b ++ "i") else "")
-- la única manera que encontré para declarar Complejo como instancia de Num
-- requiere la definición o implementación de todos estos operandos
instance Num Complejo where
  (Complejo a b) + (Complejo c d) = Complejo (a + c) (b + d)
  (Complejo a b) - (Complejo c d) = Complejo (a - c) (b - d)
  negate (Complejo a b) = Complejo a (-b) 
  (Complejo a b) * (Complejo c d) = Complejo (a*c - b*d) (a*d + b*c)
  abs (Complejo a b) = Complejo a b
  signum (Complejo a b) = 1
  fromInteger a = (Complejo 0 0)
  
--2
--i
data Direccion = Arriba | Abajo | Izquierda | Derecha
  deriving (Eq, Ord, Show)

type Coordenada = Int
type Punto = (Coordenada, Coordenada)

mover :: Punto -> Direccion -> Punto
mover (x, y) dir
  | dir == Arriba    = (x, y + 1)
  | dir == Abajo     = (x, y - 1)
  | dir == Izquierda = (x - 1, y)
  | dir == Derecha   = (x + 1, y)

--ii
mueve :: [Direccion] -> Punto -> Punto
mueve movs punto 
  | movs == []       = punto
  | otherwise        = mueve (tail movs) (mover punto (head movs)) 

--iii (hay dos apartados llamados "ii" en el enunciado)
trayectoria :: [Direccion] -> Punto -> [Punto]
trayectoria movs punto
  | movs == []          = []
  | otherwise           = (mover punto (head movs)):(trayectoria (tail movs) punto')
  where punto' = mover punto (head movs)

--iv
inferior :: [Direccion] -> [Direccion] -> Bool
inferior movs movs' = y >= y'
  where y  = foldl max 0 (map (\z -> snd z) (trayectoria movs (0, 0)))
        y' = foldl max 0 (map (\z -> snd z) (trayectoria movs' (0, 0)))


--3
data Arbol a = Hoja a | Nodo [Arbol a]  

instance Show Arbol where
   show Hoja a = show a --TODO fix: aun no funciona
-- 1. Supongamos que definimos el tipo data Pila = P [a] para representar pilas. 

data Pila a = P [a] deriving Show

-- Define funciones 
-- creaPila para crear una pila vac´ıa, 
creaPila :: Pila a
creaPila = P []

-- esPilaVacia para determinar si una pila dadaest´a vac´ıa o no, 
esPilaVacia :: Pila a -> Bool
esPilaVacia (P p) = null p

-- apliar para apilar un elemento, 
apilar :: a -> Pila a -> Pila a
apilar x (P p) = P (x:p)

-- cima para consultar la cima de una pila no vac´ıa y 
cima :: Pila a -> a
cima (P p) = head p

-- desapliar para eliminar la cima de una pila no vac´ıa. 
desapliar :: Pila a -> Pila a
desapliar (P []) = error "pila esta vacia"
desapliar (P p) = P (tail p)

-- Determina el significado de la siguiente definici´on:
r :: [a] -> [a]
r xs = ys
    where P ys = foldl (\p x -> apilar x p) creaPila xs

    --


-- 2. Define una funci´on primeroQueCumple :: (a -> Bool) ->[a] -> Maybe a 
-- que dada una propiedad y una lista devuelva el primer elemento de la lista
-- que cumple la propiedad.
-- Devuelve Nothing en el caso de que ninguno la cumpla.

primeroQueCumple :: (a -> Bool) -> [a] -> Maybe a
primeroQueCumple p xs
    | null xs == True       = Nothing
    | p (head xs) == True   = Just (head xs)
    | otherwise             = primeroQueCumple p (tail xs)


-- 3. Define un tipo de datos Cj para representar conjuntos de elementos del mismo tipo. 
data Cj a = Cjto [a] deriving Show
-- Define funciones para crear un conjunto vac´ıo, 
nuevoCjto :: Cj a 
nuevoCjto = Cjto []
-- para determinar si un conjunto dado est´a vac´ıo o no, 
cjtoVacio :: Cj a -> Bool
cjtoVacio (Cjto c) = null c
-- para determinar si un elemento pertenece o no a un conjunto y 
cjtoElem :: (Eq a) => a -> Cj a -> Bool
cjtoElem _ (Cjto [])    = False
cjtoElem x (Cjto c)     = elem x c
-- para devolver la lista con todos los elementos que pertenecen a un conjunto.
listaCjto :: (Cj a) -> [a]
listaCjto (Cjto c) = c



-- 4. Define un tipo para representar matrices de n´umeros reales. 
-- Escribe una funci´on que calcule la transpuesta de una matriz rectangular dada. 
-- Escribe una funci´on para calcular la operaci´on de suma de matrices.


-- 5. Dada la declaraci´on:
data Temp = Kelvin Float | Celsius Float | Fahrenheit Float deriving Show
-- escribe una funci´on para realizar conversiones de una escala a otra 
toCelsius :: Temp -> Float
toCelsius (Kelvin k) = k-273.15
toCelsius (Fahrenheit f) = (f-32)/(9/5)
toCelsius (Celsius c) = c

toFahrenheit :: Temp -> Float
toFahrenheit (Kelvin k) = ((9/5)*(k-273.15))+32
toFahrenheit (Celsius c) = ((9/5)*c)+32
toFahrenheit (Fahrenheit f) = f

toKelvin :: Temp -> Float
toKelvin (Kelvin k) = k
toKelvin (Celsius c) = c+273.15
toKelvin (Fahrenheit f) = (5/9)*(f-32)+273.15

normalizeKelvin :: Temp -> Temp
normalizeKelvin = Kelvin . toKelvin

normalizeCelsius :: Temp -> Temp
normalizeCelsius = Celsius . toCelsius

normalizeFahrenheit :: Temp -> Temp
normalizeFahrenheit = Fahrenheit . toFahrenheit


-- y otra para determinar la escala en la que est´a representada una temperatura. 

getScale :: Temp -> String
getScale (Fahrenheit f) = "Fahrenheit"
getScale (Celsius c) = "Celsius"
getScale (Kelvin k) = "Kelvin"

-- El nuevo tipo tiene que ser instancia de las clases Ord y Eq. 
-- Define adecuadamente los m´etodos compare y == para la nueva estructura de datos

instance Eq Temp where
    t == t' = toKelvin t == toKelvin t'

instance Ord Temp where
    t `compare` t' = (toKelvin t) `compare` (toKelvin t')


-- 6. Declara adecuadamente un tipo de datos para representar ´arboles binarios de b´usqueda
-- con valores en los nodos pero no en las hojas. Programa en Haskell la ordenaci´on de una
-- lista por el algoritmo treeSort, consistente en ir colocando uno a uno los elementos de la
-- lista en un ´arbol binario de b´usqueda inicialmente vac´ıo. 
-- A continuaci´on devuelve la lista resultante de recorrer el ´arbol en inOrden.

data Tree a = 
    Node {
        value :: a,
        left :: (Tree a),
        right :: (Tree a)
    }
    | Leaf deriving (Eq, Show)

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x (Node v l r) 
    | x == v    = Node x l r 
    | x < v     = Node v (insert x l) r
    | otherwise = Node v l (insert x r) 

inOrden :: Ord a => Tree a -> [a]
inOrden Leaf = []
inOrden (Node v Leaf Leaf) = [v]
inOrden (Node v l r) = inOrden l ++ [v] ++ inOrden r
    

treeSort :: Ord a => [a] -> [a]
treeSort [] = []
treeSort l  = inOrden (foldr insert Leaf l)


-- 7. Escribe una funci´on adivina n para jugar a adivinar un n´umero. 
-- Debe pedir que el usuario introduzca un n´umero hasta que acierte con el valor de n. 
-- Devuelve mensajes de ayuda indicando si el n´umero introducido es menor o 
-- mayor que el n´umero n a adivinar. 
-- Observaque el tipo de la funci´on ser´a adivina :: Int -> IO ().

adivina :: Int -> IO ()
adivina n = do
        putStrLn "introduce un numero: " 
        input <- getLine
        let n' = (read input :: Int)
        
        if n' == n then
            putStrLn ("enhorabuena, el numero es " ++ (show n')) 
        else if n' > n then do
            putStrLn ("el numero es menor que " ++ (show n'))
            adivina n
        else do
            putStrLn ("el numero es mayor que " ++ (show n'))
            adivina n



            
    -- foldl f x [y1, y2, .. yk]
    -- f .. (f (f x y1) y2) .. yk

    -- foldr f x [y1, y2, .. yk]
    -- f y1 (f y2 (..(f yk x)..))
    
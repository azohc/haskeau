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
data Matriz m = 
-- Escribe una funci´on que calcule la transpuesta de una matriz rectangular dada. 
-- Escribe una funci´on para calcular la operaci´on de suma de matrices.

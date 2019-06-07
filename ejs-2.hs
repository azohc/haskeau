-- 1. Escribe una funci´on zip3 :: [a] ->[a] ->[a] ->[(a,a,a)]
-- an´aloga a zip, pero que ”empareje” tres listas en lugar de dos. 
-- El n´umero de elementos de la lista resultante coincidir´a con el de la lista m´as corta.

zipTres :: [a] -> [b] -> [c] -> [(a,b,c)]
zipTres [] _ _  = []
zipTres _ [] _  = []
zipTres _ _ []  = []
zipTres (x:xs) (y:ys) (z:zs) = (x,y,z):zipTres xs ys zs


-- 2. Supongamos una representaci´on de los n´umeros racionales por medio de pares de enteros.
-- Es decir, n/m se representa como (n, m).
--     a) Escribe una funci´on simplifica que dada una fracci´on en forma de par, devuelva
--     otra equivalente lo m´as simplificada posibles. Por ejemplo: simplifica (15,9) = (5,3).
--     b) Escribe una funci´on para calcular el m´aximo com´un divisor de dos enteros positivos.

--f auxiliar: divisores 
divisores :: Int -> [Int]
divisores num = [x | x <- [1..num], num `mod` x == 0]

mcd :: Int -> Int -> Int
mcd a b = maximum [x | x <- (divisores a), x `elem` (divisores b)]

simplifica :: (Int, Int) -> (Int, Int)
simplifica (n,d)
    | mcd n d == 1  = (n,d)
    | otherwise     = (n `div` mcd n d, d `div` mcd n d)


--     c) Escribe una funci´on para calcular el m´ınimo com´un m´ultiplo de dos enteros positivos.
mcm :: Int -> Int -> Int
mcm a b = (a * b) `div` (mcd a b)

--     d) Utilizando las funciones anteriores define una funci´on para cada una de las operaciones de abajo. 
--     Todas ellas deben devolver la expresi´on simplificada de la fracci´on resultado.
--         Suma de dos n´umeros racionales: 3/4 + 5/4 = 8/4 = 2/1. 2/7 + 5/3 = 6/21 + 35/21 = 41/21
rSuma :: (Int, Int) -> (Int, Int) -> (Int, Int)
rSuma (a,b) (c,d) = simplifica (x,y)
    where { x = a*d + c*b; y = b*d }

--         Resta de dos n´umeros racionales.
rResta :: (Int, Int) -> (Int, Int) -> (Int, Int)
rResta (a,b) (c,d) = simplifica(x,y)
    where { x = a*d - c*b; y = b*d }

--         Multiplicaci´on de dos n´umeros racionales.
rMult :: (Int, Int) -> (Int, Int) -> (Int, Int)
rMult (a,b) (c,d) = simplifica(x,y)
    where { x = a*c; y = b*d }

--         Divisi´on de dos n´umeros racionales.
rDiv :: (Int, Int) -> (Int, Int) -> (Int, Int)
rDiv (a,b) (c,d) = simplifica(x,y)
    where { x = a*d; y = b*c }

--         Elevar un n´umero racional a una potencia entera (incluye negativos).
rPot :: (Int, Int) -> Int -> (Int, Int)
rPot (a,b) p = simplifica(x,y)
    where { x = a^p; y = b^p }


-- 3. Simplifica las siguientes expresiones siempre que est´en bien tipadas:
--     a) (\x y -> y x) 2
-- mal tipada: \x y: dos argumentos, solo recibe uno: 2
--     b) (\x y -> y x) 2 (\x -> x + 1)
-- (\x -> x + 1) 2
--     c) (\x -> \y -> x y) (\z -> z + 1) 2
-- (\x y -> x + 1) 2
--     d) (\x -> \y -> y/x) 2
-- mal tipada: \x -> \y ->: dos argumentos, solo recibe uno: 2
--     e) (\x y -> y * x) 2 (\x -> x + 1)
-- mal tipada: y tiene que ser Int para multiplicar. 
-- (\x -> x + 1) no recibe un argumento -> es una funcion, no un Int
--     f ) (\x y z -> y x (z x)) 2 (\x y -> y * x)
-- mal tipada: \x y z: tres argumentos, solo recibe 2: 2 (\x y -> y * x)
--     g) (\x y z -> y x (z x)) 2 (\x y -> y * x) (\x -> x + 1)
-- (\x y -> y x (x+1)) 2 (\x y -> y*x)
-- (\x -> x*(x+1)) 2 
--     h) let y = (\x -> x + 1) in y 2
-- (\x -> x + 1) 2
--     i) (\x -> x + 1) (let y = \x -> x + 1 in y 2)
-- (\x -> x + 1) 2

-- 4. Indica razonadamente cu´al es el tipo (cualificado si es necesario) de las funciones definidas
-- por las siguientes ecuaciones:
--     a) f1 x y = if x < y then x else y
-- f1 :: Ord a => a -> a -> a
--     b) f2 x y = x (y + 1)
-- f2 :: Num a => (a -> b) -> a -> b
--     c) f3 x y = (x y) + 1
-- f3 :: Num a => (b -> a) -> b -> a
--     d) f4 x y z = x y (y z)
-- f4 :: ((c -> b) -> b -> a) -> (c -> b) -> c -> a
--     e) f5 x y = (\z -> y/x - z)
-- f5 :: Fractional a => a -> a -> a -> a
--     f) f6 x y = (\z w -> w (y/x - z))
-- f6 :: Fractional a => a -> a -> a -> (a -> b) -> b
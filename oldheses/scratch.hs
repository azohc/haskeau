primo x = [y | y <-[2..x-1], mod x y == 0] == []


{-divisores :: Integer -> [Integer]
divisores n = [d | d <- [1..n], n `mod` d == 0]

ndivisores :: Integer -> Int
ndivisores n = length.divisores -- fallo aqui: 
				--scratch.hs:5:16: error:
				  --  • Couldn't match expected type ‘Int’
				    --              with actual type ‘Integer -> Int’
				    --• Probable cause: ‘(.)’ is applied to too few arguments
				      --In the expression: length . divisores
				      --In an equation for ‘ndivisores’: ndivisores n = length . divisores


esprimo :: Integer -> Bool
esprimo n 
 | n <= 1    = False
 | otherwise = ndivisores n == 2
-}

{-1. Escribe una funcion zip3 :: [a] ->[a] ->[a] ->[(a,a,a)], analoga a zip, pero que
”empareje” tres listas en lugar de dos. El numero de elementos de la lista resultante
coincidira con el de la lista m´as corta.
-}
ziptres :: [a] -> [a] -> [a] -> [(a, a, a)]
ziptres a b c
  | length a == 0 || length b == 0 || length c == 0  = []
  | otherwise = (head a, head b, head c) : ziptres (tail a) (tail b) (tail c)


{-2. Supongamos una representaci´on de los n´umeros racionales por medio de pares de enteros.
Es decir, n/m se representa como (n, m).-}

{-a) Escribe una funci´on simplifica que dada una fracci´on en forma de par, devuelva
otra equivalente lo m´as simplificada posibles. Por ejemplo: simplifica (15,9) = (5,3).-} 
reducirpar :: (Integer, Integer) -> (Integer, Integer)
reducirpar (x, y)
  | d == 1 || d == 0   = (x, y)
  | otherwise          = ((div) x d, (div) y d)
 where 
       d = (gcd) x y

{-b) Escribe una funci´on para calcular el m´aximo com´un divisor de dos enteros positivos.-}
{-mcd :: Integer Integer -> Integer
mcd a b
 | esprimo a || esprimo b = 1
 | otherwise = 0-}

--5	a)
imparesEn :: Integral a => [a] -> [a]
imparesEn x = filter odd x

--	b)
escalar :: Num a => [a] -> [a] -> a
escalar xs ys = foldl (+) 0 (zipWith (*) xs ys)

--	c)
mcdList :: Integral a => [a] -> a
mcdList [] = 0
mcdList xs = gcd (head xs) (mcdList (tail xs))


--6 a) lista de los n´umeros naturales pares menores o iguales que n.
paresHasta n = [x | x <- [1..n] , even x]

-- b) lista de los n primeros n´umeros naturales pares.
lstpares n = [x | x <- [1..2*n], even x]

-- c) lista de los pares (x,y) tales que x est´a en la lista xs, y est´a en la lista ys, y tanto x como y son n´umeros pares.

mezclaPares xs ys = [(x,y) | x <- xs, y <- ys, even x, even y] 





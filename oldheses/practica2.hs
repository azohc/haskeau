-- Juan Chozas Sumbera --

--1 
ej1a = [x*x | x <- [0..50]]
ej1a' = [x*x | x <- [0..], x <= 50]


--La lista anterior, pero con cada n´umero emparejado con su cuadrado y en orden inverso
ej1b = [(n,x) | n <- [50, 49..0], let x = n*n]

-- sumatorio i*|sen(i)|
ej1c = foldr (+) 0 [x | i <- [1..100], let x = i*(abs(sin i))]

--La lista con las 50 primeras potencias de 3. (Como antes, hazlo sin usar y usando listas infinitas. Y, para este segundo caso, hazlo usando map y usando iterate).
ej1d = [3^x | x <- [0..49]]

--segundo caso del anterior
ej1d' = take 50 (map (3^) (iterate (1+) 0))

--La suma de los n´umeros menores que 1000 que sean m´ultiplos de 3 o 5.
ej1e = [x | x <- [0..1000], (x `mod` 3 == 0) || (x `mod` 5 == 0)] 

--La lista [[1, 2, 3, 4, . . . , 20], [1, 4, 9, 16, . . . , 400], [1, 8, 27, . . . , 8000], ...
ej1f = [map (^z) x | z <- [1..10], let x = [ x | x <- [1..20]]]

--La lista [[1, 1, 1, . . . , 1], [2, 4, 8, 16, . . . , 2^10], [3, 9, 27, . . . , 3^10],
ej1g = [zipWith (^) (replicate 10 z) y | let y = [1..20], z <- [1..20]]

--La lista [1, −2, 3, −4, 5, −6, . . .]
ej1j = zipWith (*) [1..] (iterate ((-1)*) (1))


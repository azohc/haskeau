import Data.List
import System.IO

-- single line comment
{- 
multi line comment
-}

-- Bool True or False
-- Char 'a'
-- Tuples (one or more elements of different datatypes)


modExPrefix = mod 5 4   -- Operador prefijo
modExInfix = 5 `mod` 4  -- Operador infijo

-- Operaciones con numeros negativos -> (-NUM)

-- *Main> :t sqrt
-- sqrt :: Floating a => a -> a
-- a es de tipo Floating: sqrt recibe un Floating y devuelve un Floating


-- Built in maths functions
piVal = pi
ePower9 = exp 9
logOf9 = log 9
nineSquared = 9 ** 2
nineTrunc = truncate 9.999
nineRound = round 8.51      -- round 8.5 = 8
nineCeil = ceiling 8.5
nineFloor = floor 9.2

-- *Main> :t (+)
-- (+) :: Num a => a -> a -> a
-- a es Num, que puede ser Double, Float, Int, o Integer. Recibe 2 Num y devuelve uno


primeNumbers = [3,5,7,11]
morePrimes = [2] ++ primeNumbers

-- *Main> :t null
-- null :: Foldable t => t a -> Bool
-- Recibe una lista y devuelve si esta vacia 
reversePrimes = reverse morePrimes
thirdPrime = morePrimes !! 2
firstPrime = head morePrimes
lastPrime = last morePrimes
allPrimesButLast = init morePrimes
first3Primes = take 3 morePrimes
is7InPrimes = elem 7 morePrimes
maxPrime = maximum morePrimes
minPrime = minimum morePrimes
sumPrimes = sum morePrimes
productPrimes = product morePrimes

--sort recibe una lista y devuelve una lista ordenada

evenList = [0,2..10]

manyTwos = take 10 (repeat 2)
-- repeat X returns an infinte list with all elements = X

-- Listas chetadas
divisibleByXandY x y = [z | z <-[1..500], mod z x == 0, mod z y == 0]




-- PEDELELE

-- incList xs: sumar uno a todos los elementos de xs

inc :: Int -> Int
inc x = x + 1

-- incList [x1,...,xn] = [1+x1,...,1+xn]
incList :: [Int] -> [Int]
incList [] = []
incList (l:ls) = inc l:incList ls

-- lengthList xss: longitudes de los elementos de xss
-- lengthList [xs1,...,xsn] = [length xs1,...,length xsn]
lengthList :: [[Int]] -> [Int]
lengthList [] = []
lengthList (l:ls) = length l:lengthList ls


-- map resultado de aplicar f a todos los elementos de l
-- map :: (a -> b) -> [a] -> [b]

-- filter p xs = lista de elementos de xs que cumplen p
-- filter::(a -> Bool) -> [a] -> [a]

-- all p xs = todos los elementos de xs cumplen p
-- any p xs = alg´un elemento de xs cumple p
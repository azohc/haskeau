x = "pal1 pal2 pal3"

-- sublista de las n primeras palabras de una lista 
porpalabras :: (Eq a) => [a] -> b -> [a]
porpalabras x n
  | x == [] = x
  | x /= [] = porpalabras (tail x) n
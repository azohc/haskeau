-- 1
-- Escribe el tipo de las siguientes expresiones, siempre que sea posible. Escribe las que
-- sean sintacticamente correctas en notacion simplificada, sin utilizar la constructora de
-- listas (:).
    -- a) [ True : [ ]]
-- [[Bool]]
-- [[True]]

--     -- b) [ ] : [True ]
-- Type error

--     -- c) [ True ]: [ ]
-- [[Bool]]
-- [[True]]    
    
--     -- d) True : [ True ]
-- [Bool]
-- [True, True]    
    
--     -- e) 1 : (2 : 3 : [ ])
-- Num t => [t]
-- [1, 2, 3] 
    
--     -- f ) [1 : [2]] : [ [ ] ]
-- Num t => [[[t]]]
-- [[[1, 2]], []]

--     -- g) [1, 1] : (2 : [ ])
-- Type error
    
--     -- h) [ ] : [ [ ] ] : [ ]
-- [[[t]]]
-- [[], [[]]]

-- 2
-- Escribe el tipo de las siguientes expresiones, siempre que sea posible. 
-- Indica cuales estan mal tipadas y por que.
        -- a) head ['a', 'f']
-- head ['a', 'f'] :: Char
-- ret 'a'
--         -- b) tail ['a', 'f']
-- tail ['a', 'f'] :: [Char]
-- ret "f"
--         -- c) tail head "af"
-- head "af" ret 'a'
-- tail 'a' => error, espera una lista

--         -- d) head (tail "af")
-- tail "af" ret "f"
-- head "f" ret 'f'
-- head (tail "af") :: Char
    
--         -- e) splitAt 4 ['a' .. 'f']
-- splitAt 4 ['a' .. 'f'] :: ([Char], [Char])
-- ret ("abcd", "ef")    

--         -- f ) zip [3 + 2, 0] ["af"]
-- zip [3 + 2, 0] ["af"] :: [(Num, [Char])]
-- ret [(5,"af")]

--         -- g) drop (+2) [1,2,3]
-- error de tipos, esperaba otro entero para sumarlo con 2
-- drop :: Int -> [a] -> [a]

--         -- h) drop (div 2 0) [1,2,3]
-- Exception: divide by zero

--         -- i1) 'ab' ++ 'bc' 
-- Error sintáctico: 'ab' debería ser "ab"        

--         -- i2) "ab" ++ "bc" 
-- "ab" ++ "bc" :: [Char]
-- ret "abbc"

--         -- i3) "ab" + "bc" 
-- Error de tipos, + espera Int o Num
        
--         -- i4) "ab" ++ 'c'

-- Error de tipos, ++ espera dos listas


-- 3. Determina el valor de las expresiones evaluables del ejercicio anterior.

-- 4. Encuentra si es posible el valor de las siguientes expresiones
--  y explica por que no es posible en las que no se pueda.
    -- a) let x = y + 1 in let z = x ^ 2 in z
-- No hay valor para y en let x = y + 1

--     -- b) let y = let x = 2 in (let z = x ^ 2 in z) in y
-- 4

--     -- c) let y = let x = 2 in (let z = x ^ 2 in z) in z + y
-- z no está definido en in z + y

--     -- d) let {x = 5; y = 4} in if x < y then x else y
-- 4

--     -- e) let {x = 5; y = 4} in if x < y then z = x else z = y
-- No se puede dar valor a z así


--     -- f ) if [1] !! 1 == 1 then [1] else [ ]
-- Exception !! Index too large, como mucho ha de ser 0

--     -- g) let x = elem 1 [1] in if x then [1] else [ ]
-- [1]

--     -- h) let x = elem 1 [ ] in if x then [1] !! 1 else [1] !! 0
-- 1
    
--     -- i) let x = elem 1 [ ] in if x then 1 else [ ]
-- Error de tipado, lo devuelto por el if y el else debe ser del mismo tipo

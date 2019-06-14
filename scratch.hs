-- tautologia: reconoce si una fórmula es una tautologia o no: si es cierta para 
-- valores de verdad cualesquiera (True o False) de sus variables proposicionales.
tautologia :: FProp -> Bool

tautologia (V v)    = False    -- si f = v y v es Falso, f no evalua a cierto

tautologia ((V f) `Y` (No (V g))) 
        | f == g   = False     --  f ^ !f = contradiccion  
        | otherwise = True

tautologia ((V f) `O` (No (V g)))
        | f == g    = True      --  p v !p = tautologia 
        | otherwise = False

tautologia ((V f) `Y` (V g)) 
        | f == g    = tautologia (V f)      -- idempotencia
        | otherwise = False

tautologia ((V f) `O` (V g)) 
        | f == g    = tautologia (V f)      -- idempotencia
        | otherwise = False

tautologia (No (No f)) = tautologia p       -- doble negacion 

tautologia (No (f `Y` g)) = tautologia ((No f) `O` (No g))  -- De Morgan

tautologia (No (f `O` g)) = tautologia ((No f) `Y` (No g))  -- De Morgan



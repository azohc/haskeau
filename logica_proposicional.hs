-- Juan Chozas Sumbera 

type Var = String -- nombres de variables
data FProp = V Var | No FProp 
    | Y FProp FProp | O FProp FProp | Si FProp FProp | Sii FProp FProp deriving (Show, Eq)

-- f1 = !p -> (p -> (q ^ !q))
f1 = Si (No (V "p")) (Si (V "p") (Y (V "q") (No (V "q"))))

-- vars f: calcula una lista con los nombres de las variables proposicionales que hay en f
-- sin repeticiones (aunque el orden es irrelevante). vars f1 debe evaluarse a ["p","q"].

borraRepes :: Eq a => [a] -> [a]    -- data una lista, devuelve otra sin elementos repetidos
borraRepes []       = []
borraRepes (x:xs)                       
        | elem x xs = borraRepes xs     -- si la cabeza pertenece al resto se ignora
        | otherwise = x:borraRepes xs   -- si no, se agrega y se hace la llamada recursiva

vars :: FProp -> [Var]
vars (V v)      = [v]
vars (No p)     = vars p
vars (Y p q)    = borraRepes ((vars p) ++ (vars q)) -- no sabria evitar la concatenacion
vars (O p q)    = borraRepes ((vars p) ++ (vars q)) -- cuando un elemento ya pertenece a
vars (Si p q)   = borraRepes ((vars p) ++ (vars q)) -- la lista, por lo tanto creo una 
vars (Sii p q)  = borraRepes ((vars p) ++ (vars q)) -- lista y luego quito vars repetidas


-- tautologia: reconoce si una formula es una tautologia o no: si es cierta para 
-- valores de verdad cualesquiera (True o False) de sus variables proposicionales.
tautologia :: FProp -> Bool

tautologia (V v)    = False    -- si f = v y v es Falso, f no evalua a cierto

tautologia ((V p) `Y` (No (V q))) 
        | p == q    = False     --  p ^ !p = contradiccion  
        | otherwise = True

tautologia ((V p) `O` (No (V q)))
        | p == q    = True      --  p v !p = tautologia 
        | otherwise = False

tautologia ((V p) `Y` (V q)) 
        | p == q    = tautologia (V p)      -- idempotencia
        | otherwise = False

tautologia ((V p) `O` (V q)) 
        | p == q    = tautologia (V p)      -- idempotencia
        | otherwise = False

tautologia (No (No p)) = tautologia p       -- doble negacion 

tautologia (No (p `Y` q)) = tautologia ((No p) `O` (No q))  -- De Morgan

tautologia (No (p `O` q)) = tautologia ((No p) `Y` (No q))  -- De Morgan

-- satistactible: reconoce si una formula es una satisfactible o no: si es cierta
-- para algunos valores de verdad de sus variables proposicionales.

-- consecuencia: reconoce si una formula ϕ1 es consecuencia logica de otra ϕ2: 
-- si para valores de verdad cualesquiera de las variables proposicionales, 
-- cuando ϕ2 es cierta ϕ1 lo es tambien.

-- equivalente: reconoce si dos formulas ϕ1 y ϕ2 son logicamente equivalentes: 
-- si para valores de verdad cualesquiera de las variables proposicionales, 
-- cuando ϕ2 es cierta ϕ1 lo es tambien.

-- consecuencias fs: (fs lista de formulas), devuelve una lista con cada formula f de fs 
-- emparejada con la lista de aquellas formulas de fs que son consecuencia logica de f.

-- equivalentes fs: (fs lista de formulas), equivalentes fs es el conjunto cociente de fs por
-- la relacion de equivalencia logica, es decir, es una particion de fs en sublistas, 
-- cada una de las cuales esta formada por formulas de fs equivalentes entre si.
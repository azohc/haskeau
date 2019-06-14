-- Juan Chozas Sumbera
-- Junio 2019

--------------------------------------------------------------------------------------------- \
-- tipos de datos

type Var = String -- nombres de variables
data FProp = V Var | No FProp 
    | Y FProp FProp | O FProp FProp | Si FProp FProp | Sii FProp FProp deriving (Eq)

instance Show FProp where
        show (V v)      = v
        show (No (V v)) = "!" ++ v
        show (No f)     = "!(" ++ (show f) ++ ")"
--------------------------------------------------------------------------------------------- \
-- fórmulas

-- f1 = !p -> (p -> (q ^ !q))
f1 = Si (No (V "p")) (Si (V "p") (Y (V "q") (No (V "q"))))
-- f2 = !((p v q) v q)
f2 = No (((V "p") `O` (V "q")) `O` (V "q"))

--------------------------------------------------------------------------------------------- \
-- funciones

-- vars f: calcula una lista con los nombres de las variables proposicionales que hay en f
-- sin repeticiones (aunque el orden es irrelevante). vars f1 debe evaluarse a ["p","q"].

elimrepetidos :: Eq a => [a] -> [a]    -- dada una lista, devuelve otra sin elementos repetidos
elimrepetidos []    = []
elimrepetidos (x:xs)                       
        | elem x xs = elimrepetidos xs     -- si la cabeza pertenece al resto se ignora
        | otherwise = x:elimrepetidos xs   -- si no, se agrega y se hace la llamada recursiva

vars :: FProp -> [Var]
vars (V p)       = [p]
vars (No f)      = vars f
vars (f `Y` g)   = elimrepetidos ((vars f) ++ (vars g)) -- no sabria evitar la concatenacion
vars (f `O` g)   = elimrepetidos ((vars f) ++ (vars g)) -- cuando un elemento ya pertenece a la 
vars (f `Si` g)  = elimrepetidos ((vars f) ++ (vars g)) -- lista, por lo tanto creo una lista
vars (f `Sii` g) = elimrepetidos ((vars f) ++ (vars g)) -- y luego quito las vars repetidas



-- comprueba si una fórmula es atómica (si es una variable o la negación de una variable)
atomica :: FProp -> Bool
atomica (V v)         = True
atomica (No (V (v)))  = True
atomica f               = False

-- constructura de tableaux: dada una fórmula devuelve una lista de listas de fórmulas
-- cada lista contenida en la lista devuelta representa una rama
tableauxiza :: FProp -> [FProp] 

tableauxiza (V p)       = [V p]
tableauxiza (No (V p))  = [No (V p)]

-- formulas conjuntivas -- 
tableauxiza (f `Y` g)        = (tableauxiza f) ++ (tableauxiza g)
tableauxiza (No (f `O` g))   = (tableauxiza (No f)) ++ (tableauxiza (No g)) 
tableauxiza (No (f `Si` g))  = (tableauxiza f) ++ (tableauxiza (No g)) 
tableauxiza (f `Sii` g)      = (tableauxiza (f `Si` g)) ++ (tableauxiza (g `Si` f)) 


-- formulas disyuntivas
-- tableauxiza (f `O` g) 


-- formulas simplificables
tableauxiza (No (No f)) = tableauxiza f




--------------------------------------------------------------------------------------------- \


-- satistactible: reconoce si una fórmula es una satisfactible o no: si es cierta
-- para algunos valores de verdad de sus variables proposicionales.

-- consecuencia: reconoce si una fórmula ϕ1 es consecuencia logica de otra ϕ2: 
-- si para valores de verdad cualesquiera de las variables proposicionales, 
-- cuando ϕ2 es cierta ϕ1 lo es tambien.

-- equivalente: reconoce si dos fórmulas ϕ1 y ϕ2 son logicamente equivalentes: 
-- si para valores de verdad cualesquiera de las variables proposicionales, 
-- cuando ϕ2 es cierta ϕ1 lo es tambien.

-- consecuencias fs: (fs lista de fórmulas), devuelve una lista con cada fórmula f de fs 
-- emparejada con la lista de aquellas fórmulas de fs que son consecuencia logica de f.

-- equivalentes fs: (fs lista de fórmulas), equivalentes fs es el conjunto cociente de fs por
-- la relacion de equivalencia logica, es decir, es una particion de fs en sublistas, 
-- cada una de las cuales esta formada por fórmulas de fs equivalentes entre si.
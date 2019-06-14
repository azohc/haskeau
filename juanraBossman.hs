
-- ===========================================
-- ====          Tipos de datos           ====
-- ===========================================

type Var   = String  -- nombres de variables
data FProp = V Var | No FProp | Y FProp FProp | O FProp FProp | Si FProp FProp | Sii FProp FProp


-- ===========================================
-- ====             Fórmulas              ====
-- ===========================================

f1 = Si (No (V "p")) (Si (V "p") (Y (V "q") (No (V "q"))))
f2 = Y (V "p") (Si (No (V "q")) (No (V "p")))
f3 = Y (Y (V "p") (V "q")) (O (No (V "q")) (V "r"))
f4 = Sii (O (No (V "a")) (V "a")) (V "z")
f5 = Y (V "p") (No (V "p"))


-- ===========================================
-- ====            Instancias             ====
-- ===========================================

-- Igualdad estructural excepto en las operaciones /\ y \/,
-- en las cuales el orden de los operandos no importa
instance Eq FProp where
    (V var1) == (V var2)                   = var1 == var2
    (No prop1) == (No prop2)               = prop1 == prop2
    (Y prop1 prop2) == (Y prop3 prop4)     = (prop1 == prop3 && prop2 == prop4) ||
                                             (prop1 == prop4 && prop2 == prop3)
    (O prop1 prop2) == (O prop3 prop4)     = (prop1 == prop3 && prop2 == prop4) ||
                                             (prop1 == prop4 && prop2 == prop3)
    (Si prop1 prop2) == (Si prop3 prop4)   = prop1 == prop3 && prop2 == prop4
    (Sii prop1 prop2) == (Sii prop3 prop4) = prop1 == prop3 && prop2 == prop4
    p == p'                                = False

-- Cuidado con el orden: a será menor que b si b -> a
instance Ord FProp where
    a <= b = consecuencia b a

instance Show FProp where
    show (V var)           = var
    show (No fun)          = "~(" ++ show fun ++ ")"
    show (Y prop1 prop2)   = "(" ++ show prop1 ++ ") /\\ (" ++ show prop2 ++ ")"
    show (O prop1 prop2)   = "(" ++ show prop1 ++ ") \\/ (" ++ show prop2 ++ ")"
    show (Si prop1 prop2)  = "(" ++ show prop1 ++ ") -> (" ++ show prop2 ++ ")"
    show (Sii prop1 prop2) = "(" ++ show prop1 ++ ") <-> (" ++ show prop2 ++ ")"

-- ===========================================
-- ====      Funciones principales        ====
-- ===========================================

-- Obtiene una lista con las distintas variables que contiene la fórmula
-- · Primero obtiene la lista completa de variables (puede haber repeticiones)
-- · Después las filtra eliminando las repetidas.
vars :: FProp -> [Var]
vars f = unique (getVars f)

-- Comprueba que, para cualquier combinación de valores posible, f siempre es True
tautologia :: FProp -> Bool
tautologia f = all (== True) [eval f vl | vl <- allPossibleValues (vars f)]

-- Comprueba que existe al menos una combinación de valores para la cual f es True
satisfactible :: FProp -> Bool
satisfactible f = True `elem` [eval f vl | vl <- allPossibleValues (vars f)]

-- Para todos los valores con los que f es cierta, g debe serlo también (f -> g)
-- La lista de variables sobre la que operar debe de ser las uniones de f y g.
consecuencia :: FProp -> FProp -> Bool
consecuencia f g = all (== True) [implies f g vl | vl <- allPossibleValues (unique (vars f ++ vars g))]

-- Para cualquier conjunto de valores, f y g deben tener el mismo resultado (f <-> g)
equivalente :: FProp -> FProp -> Bool
equivalente f g = all (== True) [equiv f g vl | vl <- allPossibleValues (unique (vars f ++ vars g))]

-- Devuelve una lista de pares (f, cs) siendo cs todas las funciones consecuencia de f
consecuencias :: [FProp] -> [(FProp, [FProp])]
consecuencias fs = [(f, cs) | f <- fs, let cs = [c | c <- fs, c /= f && consecuencia f c], not (null cs)]

-- Divide la lista fs en sublistas con todos sus elementos equivalentes entre sí
-- · Para ello primero crea tantas listas de funciones equivalentes como funciones reciba,
-- por ello si f y g son equivalentes ambas apareceran tanto en la lista de f como en la lista de g
-- · Por último se eliminan los elementos repetidos de esta lista de listas,
-- es decir, aquellas listas que ya tenemos de forma idéntica.
equivalentes :: [FProp] -> [[FProp]]
equivalentes fs = unique [[e | e <- fs, equivalente f e] | f <- fs]


-- ===========================================
-- ====       Funciones auxiliares        ====
-- ===========================================

-- Avanza recursivamente hasta encontrar las variables y las devuelve concatenadas
-- Devuelve variables repetidas en caso de aparecer más de una vez
getVars :: FProp -> [Var]
getVars (V var)         = [var]
getVars (No fun)        = getVars fun
getVars (Y fun1 fun2)   = getVars fun1 ++ getVars fun2
getVars (O fun1 fun2)   = getVars fun1 ++ getVars fun2
getVars (Si fun1 fun2)  = getVars fun1 ++ getVars fun2
getVars (Sii fun1 fun2) = getVars fun1 ++ getVars fun2

-- Filtra una lista eliminando los elementos duplicados
-- Para cada elemento añadido, lo concatena con la aplicación de unique sobre
-- el resto de la lista, a excepción de las repeticiones del valor añadido
unique :: Eq a => [a] -> [a]
unique []       = []
unique (x : xs) = x : unique [y | y <- xs, y /= x]
-- Versión alternativa usando filter en lugar de listas intensionales
-- unique (x : xs) = x : unique (filter (x /=) xs)

-- Evalua una expresión en base a unos valores para sus variables
-- Una vez llega al nivel de variable "itera" sobre la lista hasta
-- encontrar su valor (p.ej. ("p", True))
eval :: FProp -> [(Var, Bool)] -> Bool
eval (V var) ((x, y):xs) | var == x  = y
                         | otherwise = eval (V var) xs
eval (No prop) vl                    = not (eval prop vl)
eval (Y prop1 prop2) vl              = eval prop1 vl && eval prop2 vl
eval (O prop1 prop2) vl              = eval prop1 vl || eval prop2 vl
eval (Si prop1 prop2) vl             = implies prop1 prop2 vl
eval (Sii prop1 prop2) vl            = equiv prop1 prop2 vl

-- Transformación booleada de f -> g
implies :: FProp -> FProp -> [(Var, Bool)] -> Bool
implies f g vl = not (eval f vl) || eval g vl

-- Transformación booleana de f <-> g
equiv :: FProp -> FProp -> [(Var, Bool)] -> Bool
equiv f g vl = implies f g vl && implies g f vl

-- Genera todos los valores booleanos posibles para un conjunto de variables
-- Fuente: https://stackoverflow.com/a/29713381/7809508
allPossibleValues :: [Var] -> [[(Var, Bool)]]
allPossibleValues = foldr (\x xs -> (:) <$> [(x,True),(x,False)] <*> xs) [[]]
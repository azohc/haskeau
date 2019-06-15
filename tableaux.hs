-- Juan Chozas Sumbera

--------------------------------------------------------------------------------------- \\
-- Tipos de datos

data FProp = Cierto | Falso | P String | No FProp | Y FProp FProp | O FProp FProp deriving Read

instance Eq FProp where
    Cierto == Cierto            = True
    Falso == Falso              = True
    (P p) == (P p')             = p == p'
    (No f) == (No f')           = f == f'
    (f `Y` g) == (f' `Y` g')    = (f == f' && g == g') || (f == g' && g == f')
    (f `O` g) == (f' `O` g')    = (f == f' && g == g') || (f == g' && g == f')
    f == f'                     = False

instance Show FProp where
    show Cierto         = "⊤"
    show Falso          = "⊥"
    show (P p)          = p
    show (No f)         = "¬" ++ (show f)
    show (f `Y` g)      = "(" ++ (show f) ++ " ∧ " ++ (show g) ++ ")"
    show (f `O` g)      = "(" ++ (show f) ++ " ∨ " ++ (show g) ++ ")"

instance Ord FProp where
    f <= f' = consecuencia f' [f]

data Tableau =      -- estructura arbórea para representar un tableau
    Unario {        -- nodos de un hijo: conjunciones o simplificaciones de doble negación
        f :: FProp,
        hijo :: Tableau
    }
    | Binario {     -- nodos de dos hijos: para disyunciones
        f :: FProp,
        hizq :: Tableau,
        hder :: Tableau
    }  
    | Hoja deriving (Show, Eq)


--
------------------------------------------------------------------------------------- \\
-- Fórmulas

-- f1 ≡ (p ∧ ¬q) ∨ ¬p
f1 = O (Y (P "p") (No (P "q"))) (No (P "p"))
-- f2 ≡ ¬⊥ ∨ (p ∧ ¬(q ∨ ¬q))
f2 = (No Falso) `O` ((P "p") `Y` (No ((P "q") `O` (No (P "q")))))
-- f3 ≡ ⊤ ∧ q ∧ (¬q ∨ r)
f3 = Cierto `Y` ((P "q") `Y` ((No (P "q")) `O` (P "r")))
-- f4 ≡ ¬(p ∧ (p ∨ r)
f4 = No ((P "p") `Y` ((P "p") `O` (P "r")))
-- f5 ≡ ¬((r ∨ p) ∧ p)
f5 = No (((P "r") `O` (P "p")) `Y` (P "p"))
 
--------------------------------------------------------------------------------------- \\
-- Funciones

-- tableau_cerrado: dado un conjunto de fórmulas devuelve cierto o falso
-- según sea posible o no encontrar un tableau cerrado para dicho conjunto

tableau_cerrado :: [FProp] -> Bool
tableau_cerrado fs = all rama_cerrada (map (filter atomica) (ramas $ tableau fs))


-- tautologia: reconoce si una fórmula es una tautología o no. 
-- Es decir, si su negación tiene un tableau cerrado y por tanto es insatisfactible.

tautologia :: FProp -> Bool
tautologia f = tableau_cerrado [No f]


-- consecuencia: reconoce si una fórmula ϕ es consecuencia lógica de un conjunto de fórmulas Φ.
-- Es decir, si es posible encontrar un tableau cerrado para Φ ∪ {¬ϕ}.

consecuencia :: FProp -> [FProp] -> Bool
consecuencia f fs = tableau_cerrado $ (No f):fs


-- equivalentes: reconoce si dos fórmulas ϕ1 y ϕ2 son lógicamente equivalentes. 
-- Es decir, la fórmula (¬ϕ1 ∨ ϕ2) ∧ (¬ϕ2 ∨ ϕ1) es una tautología.

equivalentes :: FProp -> FProp -> Bool
equivalentes f g = tautologia $ ((No f) `O` g) `Y` ((No g) `O` f) 


--------------------------------------------------------------------------------------- \\
-- Auxiliares


-- dada una lista de fórmulas, devuelve un tableau que se desarrolla recursivamente
tableau :: [FProp] -> Tableau   

tableau [] = Hoja -- caso base, tableau vacío

-- elementos atómicos (no reducibles)
tableau ((P p):xs)          = Unario (P p) (tableau xs)
tableau ((No (P p)):xs)     = Unario (No (P p)) (tableau xs)
tableau (Cierto:xs)         = Unario Cierto (tableau xs)
tableau (Falso:xs)          = Unario Falso (tableau xs)

-- inversas de tautología y contradicción
tableau ((No Cierto):xs)    = Unario Falso (tableau xs)
tableau ((No Falso):xs)     = Unario Cierto (tableau xs)

-- fórmulas simplificables  : expanden la fórmula en la rama actual
tableau ((No (No f)):xs)    = Unario (No (No f)) (tableau (f:xs))

-- fórmulas conjuntivas     : expanden la fórmula en la rama actual
tableau ((f `Y` g):xs)      = Unario (f `Y` g) (tableau (f:g:xs))
tableau ((No (f `O` g)):xs) = Unario (No (f `O` g)) (tableau ((No f):(No g):xs))

-- fórmulas disyuntivas     : al expandir la fórmula la rama actual se abre en dos 
tableau ((f `O` g):xs)      = Binario (f `O` g) (tableau (f:xs)) (tableau (g:xs))
tableau ((No (f `Y` g)):xs) = Binario (No (f `Y` g)) (tableau ((No f):xs)) (tableau ((No g):xs))


-- dado un tableau, devuelve una lista de listas de fórmulas
-- cada lista perteneciente a la lista devuelta representa una rama del tableau
-- https://stackoverflow.com/questions/55662192/building-a-list-of-all-branches-in-a-tree
ramas :: Tableau -> [[FProp]]
ramas Hoja = [[]]
ramas (Unario f h) = map (f:) (ramas h)
ramas (Binario f i d) = map (f:) $ (ramas i) ++ (ramas d)


-- comprueba si una fórmula es atómica (no reducible)
atomica :: FProp -> Bool
atomica (P f)       = True
atomica (No (P f))  = True
atomica Cierto      = True
atomica Falso       = True
atomica f           = False


-- comprueba si una rama es cerrada
rama_cerrada :: [FProp] -> Bool

rama_cerrada [] = False

rama_cerrada ((P p):xs)         -- si p y ¬p están en la misma rama es cerrada
    | elem (No (P p)) xs    = True
    | otherwise             = rama_cerrada xs

rama_cerrada ((No (P p)):xs)    -- si ¬p y p están en la misma rama es cerrada
    | elem (P p) xs         = True
    | otherwise             = rama_cerrada xs

rama_cerrada (Cierto:xs)        -- si Cierto y Falso están en la misma rama es cerrada
    | elem Falso xs         = True
    | elem (No Cierto) xs   = True
    | otherwise             = rama_cerrada xs
    
rama_cerrada (Falso:xs)         -- si Falso y Cierto están en la misma rama es cerrada
    | elem Cierto xs        = True
    | elem (No Falso) xs    = True
    | otherwise             = rama_cerrada xs



--------------------------------------------------------------------------------------- \\
-- Entrada/salida

leeFormula :: IO (FProp)
leeFormula = do
    o <- getLine
    
    putStrLn ("Escribe una formula utilizando los siguientes operadores:")
    putStrLn ("- Cierto:\trepresenta la constante cierto")
    putStrLn ("- Falso:\trepresenta la constante falso")
    putStrLn ("- P v:   \tv (String) representa una variable")
    putStrLn ("- No f:  \tnegacion de la formula f")
    putStrLn ("- Y f g:\tconjuncion de las formulas f y g")
    putStrLn ("- O f g:\tdisyuncion de las formulas f y g")
    
    input <- getLine
    let formula = read input :: FProp
    putStr . show $ formula
    putStrLn " es la formula interpretada\n"

    return formula

main = do
    putStrLn ""
    putStrLn "1. Imprimir el tableau de un conjunto de formulas"
    putStrLn "2. Comprobar si un conjunto de formulas produce un tableau cerrado"
    putStrLn "3. Comprobar si una formula es tautologia"
    putStrLn "4. Comprobar si una formula es consecuencia logica de un conjunto de formulas"
    putStrLn "5. Comprobar si dos formulas son equivalentes"
    putStrLn "Que quieres hacer? (1,2,3,4 o 5)"

    o <- getLine
    let opcion = read o :: Int
    case opcion of
        1 -> do 
            putStrLn ""
            let formulas = leeFormula:[]
            -- leer mas formulas si se solicita
            -- print (tableau formulas)
            putStrLn "..." -- convertir IO FProp a FProp

        2 -> do
            putStrLn ""
            let formulas = leeFormula:[]
            -- leer mas formulas si se solicita
            -- print (tableau_cerrado (tableau formulas))
            putStrLn "..." -- convertir IO FProp a FProp

        3 -> do
            putStrLn ""
            let formula = leeFormula
            -- print (tautologia formula)
            putStrLn "..." -- convertir IO FProp a FProp
        
        4 -> do
            putStrLn ""
            let f = leeFormula
            let fs = leeFormula:[]
            -- leer mas formulas si se solicita
            -- print (consecuencia f fs)
            putStrLn "..." -- convertir IO FProp a FProp

        5 -> do
            putStrLn ""
            let f = leeFormula
            let g = leeFormula
            -- leer mas formulas si se solicita
            -- print (equivalentes f g)
            putStrLn "..." -- convertir IO FProp a FProp
                
        _ -> putStr "error de entrada\n"
            

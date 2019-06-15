



data Formula = T | F | P String | NOT Formula | AND Formula Formula | OR Formula Formula

instance Show Formula where
    show T              = "T"
    show F              = "F"
    show (P v)          = v
    show (NOT f)        = "¬"  ++ (show f)   
    show (f `AND` g)    = "("  ++ (show f) ++ " ∧ "   ++ (show g) ++ ")"
    show (f `OR` g)     = "("  ++ (show f) ++ " ∨ "   ++ (show g) ++ ")"

instance Eq Formula where
    T == T                        = True
    F == F                        = True
    (P p) == (P p')               = p == p'
    (NOT f) == (NOT f')           = f == f'
    (f `AND` f') == (g `AND` g')  = (f == g && f' == g') || (f == g' && f' == g)
    (f `OR` f') == (g `OR` g')    = (f == g && f' == g') || (f == g' && f' == g)
    f == f'                       = False

    
--------------------------------------------------------------------------------------------- \
-- FORRMULAS

-- f1 ≡ (p ∧ ¬q) ∨ ¬p
f1 = OR (AND (P "p") (NOT (P "q"))) (NOT (P "p"))
-- f2 ≡ ¬⊥ ∨ (p ∧ ¬(q ∨ ¬q))
f2 = (NOT F) `OR` ((P "p") `AND` (NOT ((P "q") `OR` (NOT (P "q")))))
-- f3 ≡ T ∧ q ∧ (¬q ∨ r)
f3 = T `AND` ((P "q") `AND` ((NOT (P "q")) `OR` (P "r")))
-- f4 ≡ ¬(p ∧ (p ∨ r))
f4 = NOT ((P "p") `AND` ((P "p") `OR` (P "r")))
-- f5 ≡ ¬((r ∨ p) ∧ p)
f5 = NOT (((P "r") `OR` (P "p")) `AND` (P "p"))

fa = ((P "a") `AND` (P "b"))

fb = (((P "a") `OR` (P "c")) `AND` (P "b"))

fc = ((P "a") `OR` (P "b"))


--------------------------------------------------------------------------------------------- \
-- FUNCIORNES

-- tableau_cerrado que dado un conjunto de fórmulas devuelva T o F
-- tableau_cerrado :: [Formula] -> Bool
-- tableau_cerrado phi = do


-- Utiliza la tableau_cerrado anterior para programar las siguientes funciones:

-- tautologia: reconoce si una fórmula es una tautología o no. 
-- Es decir, si su negación tiene un tableau cerrado y por tanto es insatisfactible.

-- consecuencia: reconoce si una fórmula ϕ es consecuencia lógica de un conjunto de fórmulas Φ. 
-- Es decir, si es posible encontrar un tableau cerrado para Φ ∪ {¬ϕ}.

-- equivalentes: reconoce si dos fórmulas ϕ1 y ϕ2 son lógicamente equivalentes. 
-- Es decir, la fórmula (¬ϕ1 ∨ ϕ2) ∧ (¬ϕ2 ∨ ϕ1) es una tautología.




--------------------------------------------------------------------------------------------- \
-- AUXILIARES

-- constructura de tableaux: dada una fórmula devuelve una lista de listas de fórmulas
-- cada lista contenida en la lista devuelta representa una rama
make_tableaux :: Formula -> [[Formula]] 

make_tableaux (P p)       = [[P p]]
make_tableaux (NOT (P p))  = [[NOT (P p)]]

-- conjunction  : should add (make_tableaux f) and (make_tableaux g) 
--                      to an inner list, not create a new one
make_tableaux (f `AND` g)   = [(f `AND` g):(concat ((make_tableaux f) ++ (make_tableaux g)))]

-- disjunction  : makes another list (new branch) in the outer list
make_tableaux (f `OR` g)    = [f `OR` g]:((make_tableaux f) ++ (make_tableaux g)) 

-- simplification
make_tableaux (NOT (NOT f)) = make_tableaux f


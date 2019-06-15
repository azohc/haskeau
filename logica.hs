-- Juan Chozas Sumbera
-- Junio 2019

-- • Hay que declarar los tipos de todas las funciones que se programen, incluidas las funciones
-- auxiliares que pudieran necesitarse.
-- • Se pueden usar todas las funciones de Prelude, es decir, las que se cargan con el sistema,
-- pero no se puede importar ningún otro módulo, lo que quiere decir que, por ejemplo, si usan
-- operaciones con listas que no están en Prelude hay que programarlas.
-- • Para poder realizar ejemplos y evaluar la práctica, deben incluirse al menos cinco fórmulas
-- proposicionales concretas (f1,f2,f3,f4,f5), definidas mediante funciones de aridad 0 (al
-- estilo de la f1 de más arriba). Las tres primeras, f1,f2,f3 deben corresponder a las fórmulas
-- f1, f2, f3 de arriba.



-- Declarar FProp como instancia de las siguientes clases de tipos:
--     • Como instancia de la clase Eq, haciendo que la igualdad entre fórmulas coincida con 
--     la igualdad estructural (es decir, componente a componente), salvo por el hecho de 
--     que el orden en conjunciones o disyunciones no importe. Por ejemplo, la fórmula 
--     ¬(p ∧ (p ∨ r)) sería igual a la fórmula ¬((r ∨ p) ∧ p).

--     • Como instancia de la clase Ord, de modo que una fórmula ϕ 
--     sea menor que otra ϕ 0 si ϕ 0 es consecuencia lógica de ϕ.

--------------------------------------------------------------------------------------------- \
-- TIPOS DE DATOS


data FProp = Cierto | Falso | P String | No FProp | Y FProp FProp | O FProp FProp deriving Read


instance Show FProp where
    show Cierto         = "T"
    show Falso          = "F"
    show (P v)          = v
    show (No f)         = "¬"  ++ (show f)   
    show (f `Y` g)      = "("  ++ (show f) ++ " ∧ "   ++ (show g) ++ ")"
    show (f `O` g)      = "("  ++ (show f) ++ " ∨ "   ++ (show g) ++ ")"


    
--     ¬(p ∧ (p ∨ r)) sería igual a la fórmula ¬((r ∨ p) ∧ p).
instance Eq FProp where
    Cierto == Cierto            = True
    Falso == Falso              = True
    (P p) == (P p')             = p == p'
    (No f) == (No f')           = f == f'
    (f `Y` f') == (g `Y` g')    = (f == g && f' == g') || (f == g' && f' == g)
    (f `O` f') == (g `O` g')    = (f == g && f' == g') || (f == g' && f' == g)
    f == f'                     = False

    
--------------------------------------------------------------------------------------------- \
-- FORMULAS

-- f1 ≡ (p ∧ ¬q) ∨ ¬p
f1 = O (Y (P "p") (No (P "q"))) (No (P "p"))
-- f2 ≡ ¬⊥ ∨ (p ∧ ¬(q ∨ ¬q))
f2 = (No Falso) `O` ((P "p") `Y` (No ((P "q") `O` (No (P "q")))))
-- f3 ≡ T ∧ q ∧ (¬q ∨ r)
f3 = Cierto `Y` ((P "q") `Y` ((No (P "q")) `O` (P "r")))
-- f4 ≡ ¬(p ∧ (p ∨ r))
f4 = No ((P "p") `Y` ((P "p") `O` (P "r")))
-- f5 ≡ ¬((r ∨ p) ∧ p)
f5 = No (((P "r") `O` (P "p")) `Y` (P "p"))

fa = ((P "a") `Y` (P "b"))

fb = (((P "a") `O` (P "c")) `Y` (P "b"))

fc = ((P "a") `O` (P "b"))


--------------------------------------------------------------------------------------------- \
-- FUNCIONES

-- tableau_cerrado que dado un conjunto de fórmulas devuelva cierto o falso
-- tableau_cerrado :: [FProp] -> Bool
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
tableauxiza :: FProp -> [[FProp]] 

tableauxiza (P p)       = [[P p]]
tableauxiza (No (P p))  = [[No (P p)]]

-- reglas conjuntivas --     debería meterse la descomposicion de f y g en una misma lista
tableauxiza (f `Y` g)       = (tableauxiza f) ++ (tableauxiza g) -- con ++ no puede ser...
tableauxiza (No (f `O` g))  = (tableauxiza (No f)) ++ (tableauxiza (No g))-- ...crearia otra lista

-- reglas disyuntivas        crear otra lista para descomponer f y g en distintas ramas
tableauxiza (f `O` g)       = (tableauxiza f) ++ (tableauxiza g)
tableauxiza (No (f `Y` g))  = (tableauxiza (No f)) ++ (tableauxiza (No g))

-- reglas simplificación
tableauxiza (No (No f)) = tableauxiza f


-- comprueba si una fórmula es atómica 
atomica :: FProp -> Bool
atomica Cierto          = True
atomica Falso           = True
atomica (P p)           = True
atomica (No (P (p)))    = True
atomica f               = False


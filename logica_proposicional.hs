-- Juan Chozas Sumbera 

type Var = String -- nombres de variables
data FProp = V Var | No FProp | Y FProp FProp 
        | O FProp FProp | Si FProp FProp | Sii FProp FProp deriving Show

-- f1 = !p -> (p -> (q v !q))
-- f1 = Si (No (V "p")) (Si (V "p") (Y (V "q") (No (V "q"))))
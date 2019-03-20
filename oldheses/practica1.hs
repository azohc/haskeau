--Adrian Melero y Juan Chozas

ej15_1 = last [1..10^5]  --Poco (0.00 secs)
ej15_2 = last [1..10^7]  --Regular (0.12 secs)
ej15_3 = last [1..10^20] --Mucho (interrumpido)
ej15_4 = head [1..10^20] --Poco (0.02 secs)
ej15_5 = last [10^20..1] --Error
ej15_6 = head (tail [1..10^20]) --Poco (0.00 secs)
ej15_7 = length [1..10^20] --Mucho (Interrumpido)
ej15_8 = last (take (10^7) [1..10^20]) --Regular (0.17 secs)
ej15_9 = head (take (10^7) ([1..100] ++ [1..10^20])) -- Poco (0.00 secs)
ej15_10 = last (take 100 ([1..10^20] ++ [1..100])) --Poco (0.00 secs)
ej15_11 = last (drop 100 ([1..10^20] ++ [1..100])) -- Mucho (Interrumpido)
ej15_12 = head (drop (10^7) ([1..10^20] ++ [1..100])) --Regular (0.17 secs)
ej15_13 = [1..10^7]==[1..10^7] --Regular (0.28 secs)
ej15_14 = [1..10^20]==[1..10^20] --Mucho (Interrumpido)
ej15_15 = [1..10^20]==[1..10^20+1] --Mucho (Interrumpido)
ej15_16 = [1..10^20]==[2..10^20] --Poco (0.00 secs)
ej15_17 = head (reverse [1..10^7]) --Regular (0.92 secs)
ej15_18 = last (reverse [1..10^7]) --Regular (0.94 secs)
ej15_19 = reverse [1..10^20] == reverse [1..10^20+1] --Mucho (Interrumpido)


ej1_2 = 10^10 `div` (60 * 60 * 24 * 365) -- 317



ej1_3 = let 
           anios = ej1_2
           seg_dias = 10^10 - (60 * 60 * 24 * 365 * ej1_2)
           dias = seg_dias `div` (60 * 60 * 24)
           seg_horas = seg_dias - (60 * 60 * 24 * dias)
           horas = seg_horas `div` (60 * 60)
           seg_segs = seg_horas - (60 * 60  * horas)
           segs = seg_segs `div` (60)
           
        in (anios, dias, horas, segs)   --(317,35,17,46)



-- ejercicio 2
ej2_f x y = 2 * x - y * x      -- f(7,4) = -14

ej2_g x = ej2_f (ej2_f 2 x) (ej2_f x 1)    -- f(f(1,1),g(-2)) = -30

ej2_h x y z = ej2_f (ej2_f (x + 2 * y) (ej2_g 3)) (5 - y - ej2_g z)  --siempre 0

ej2_i x y                                    --(i 2 2) - i (g 1) 0 = 2
    | x >= y && y > 0  = x - y
    | y > x && x > 0   = 0
    | otherwise        = y - x

-- con operadores infijos
ej2_i' x y =
    if (>=) x y && (>) y 0      then (-) x y
    else if (>) y x && (>) x 0  then 0
    else                        (-) y x

ej2_f' x y = (-) ((*) 2 x) ((*) y x)

ej2_h' x y z = ej2_f (ej2_f ((+) x ((*) 2  y)) (ej2_g 3)) ((-) ((-) 5 y) (ej2_g z))

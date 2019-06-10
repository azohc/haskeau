-- Calcula cu´antos a˜nos hay en 10^10 segundos 
-- (sup´on que todos los a˜nos tienen 365 d´ıas; en otro
-- momento puedes hacerlo teniendo en cuenta bisiestos).

sec2min :: Fractional a => a -> a
sec2min s = (/) s 60

min2hr :: Fractional a => a -> a
min2hr m = (/) m 60

hr2day :: Fractional a => a -> a
hr2day h = (/) h 24

day2yr :: Fractional a => a -> a
day2yr d = (/) d 365

-- respuesta con day2yr $ hr2day $ min2hr $ sec2min (10 ** 10)
sec2yr :: Fractional a => a -> a
sec2yr s = day2yr $ hr2day $ min2hr $ sec2min s

-- Calcula cu´antos a˜nos enteros, d´ıas restantes enteros,
--  horas restantes enteras, minutos restantes
-- enteros y segundos restantes hay en 1010 segundos
sec2qtuple :: Int -> (Int,Int,Int,Int,Int)
sec2qtuple s = (y,d,h,m,r) 
    where 
        m'  = div s 60
        h'  = div m' 60
        d'  = div h' 24
        y   = div d' 365
        d   = d' - (y * 365)
        h   = h' - (d' * 24)
        m   = m' - (h' * 60)
        r   = s - (m' * 60)



asdf
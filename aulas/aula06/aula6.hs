tringulo a b c | (a+b) <= c || (a+c) <= b || (c+b) <= a = "NAO EH TRI"
 | a==b && b==c = "Equilatero"
 | a==b || a==c || b==c = "Isoceles"
 | otherwise = "Escaleno"



(*-*) :: Bool -> Bool -> Bool

False *-* False = False
_ *-* _ = True


fatorial :: Int -> Int

fatorial n | n==1 = 1
 | otherwise = fatorial(n-1)*n 


fat :: (Integral a, Ord a) => a -> a
fat 0 = 1
fat b = fat (b-1) * b

--poow Int Int -> Int
poow _ 0 = 1
poow 0 _ = 0
poow x y = x * poow x (y-1)

ultimo :: [t] -> t
ultimo [x] = x
ultimo (_:xs) = ultimo xs


inicio :: [t] -> [t]
inicio [] = []
inicio [x] = []
inicio (x:xs) = x: inicio xs






 

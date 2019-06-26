twice :: (a -> a) -> a -> a
twice f x = f (f x)

map1 :: (a -> b) -> [a] -> [b]
map1 f xs = [f(y) | y <- xs]

map2 :: (a -> b) -> [a] -> [b]
map2 f [] = []
map2 f (x:xs) = f x:map2 f xs 

filtro1 :: (a-> Bool) -> [a] -> [a]
filtro1 f xs = [y|y<-xs, f(y)]

filtro2 :: (a-> Bool) -> [a] -> [a]
filtro2 f [] = []
filtro2 f (x:xs) | f x = x:filtro2 f xs
 | otherwise = filtro2 f xs

paratodo1 :: (a -> Bool) -> [a] -> Bool
paratodo1 f xs = and[f(y) | y<-xs]

paratodo2 :: (a -> Bool) -> [a] -> Bool
paratodo2 f [] = True
paratodo2 f (x:xs) = f x && (paratodo2 f xs)

existe1 :: (a -> Bool) -> [a] -> Bool
existe1 f xs = or[f(y) | y<-xs]

existe2 :: (a -> Bool) -> [a] -> Bool
existe2 f [] = True
existe2 f (x:xs) = f x || (existe2 f xs)

--pegar1 :: (a->Bool) -> [a] -> [a]
--pegar1 f xs = [z | x <- xs, z = f(x) && y, ] 

pegar2 f [] = []
pegar2 f (x:xs) | f(x) = x:pegar2 f xs
 | otherwise = []

largar2 f [] = []
largar2 f (x:xs) | f(x) = largar2 f xs
 | otherwise = x:xs


impar = not.even

segundo = head.tail

terceiro = head.tail.tail

ultimo = head.reverse








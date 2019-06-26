odds n = map f [0..n-1]
 where
  f x = x^2 + 1

odds1 n = map (\x -> x*2 + 1) [0..n-1]

produto3 = \z -> (\x -> (\y -> x * y * z))

evens :: (Num a, Enum a) => a -> [a]
evens = \n -> (map (\x -> x*2) [0 .. n-1])

incremento :: Num a => a -> a
incremento = (1+)

quadrado :: Num a => a -> a
quadrado = (^2)

inverso :: Fractional a => a -> a
inverso = (1/)

dobro :: Num a => a -> a
dobro = (*2)

metade :: Fractional a => a -> a
metade = (/2)

antecessor = ((-1)+)

multiplos7 n = [x | x <-[1..n*7], x `mod` 7 == 0]
multiplos7_1 n = [x*7 | x <- [1..n]]
multiplos7_2 = \n -> (map (\x -> x*7) [1..n])

possibilidades = [(x, y) | y<-[4,5], x<-[1,2,3]]

tabuadaGeral = [(x, y, x*y) | x<-[0..9], y<-[0..9], y>=x]

indices :: (Num a, Enum a, Ord a) => a -> a -> [(a,a)]
indices m n = [(x,y) | x<-[0..m], y<-[0..n], y>=x]






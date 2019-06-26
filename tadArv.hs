import Data.List


data ArvN a = NilN
            | NoN a [ArvN a]
			deriving (Show, Eq)
			

criaRaiz [] = NilN			
criaRaiz ls = NoN (head ls) (criaFilhos (tail ls))


criaFilhos [] = []
criaFilhos ls = (NoN (head ls) [NilN]): criaFilhos (tail ls) 



buscaNo x NilN = NilN 
buscaNo x (NoN y fs) | x == y = NoN y fs
					 | otherwise = buscaNoF x fs
		
					 
buscaNoF x [] = NilN
buscaNoF x fs | buscaNo x (head fs) == NilN = buscaNoF x (tail fs)
              | otherwise = head fs
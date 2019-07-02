import System.IO
import Data.List

data ArvN a = Nil
            | No a [ArvN a] 
            deriving (Show, Eq) 
            
criaArvN [] = Nil
criaArvN (x:xs) = No x (criaArvNF xs)
            
criaArvNF [x] = [No x [Nil]]
criaArvNF (x:xs) = [No x [Nil]] ++ criaArvNF xs

buscaN x Nil = Nil
buscaN x (No y fs) | x == y = No y fs
				   | otherwise = buscaNF x fs
		
buscaNF x [] = Nil
buscaNF x fs | buscaN x (head fs) == Nil = buscaNF x (tail fs)
             | otherwise = head fs

pegaRaizArvN Nil = -1
pegaRaizArvN (No x fs) = x

insereN :: (Eq a) => ArvN a -> ArvN a -> ArvN a 
insereN arv Nil = Nil
insereN (No x fs) (No z gs) | x == z = No z (gs++fs)
                            | otherwise = No z (gs++(insereNF (No x fs) gs))
                            
insereNF arv [] = []
insereNF (No x fs) (z:zs) | buscaN x z /= Nil = [(insereN (No x fs) z)]
                          | otherwise = insereNF (No x fs) zs
                            
stringToInt xs = [read x :: Int | x<-xs]
stringsToInts xs = [stringToInt (words y) | y <- xs]

criaArvs [] = []
criaArvs xs = [criaArvN x | x<-xs]

-- LISTA (x:xs) É UMA LISTA DE ARVORES
criaMegaArv [x] Nil = x
criaMegaArv (x:xs) Nil = criaMegaArv xs x
criaMegaArv (x:xs) arv = criaMegaArv xs (insereN x arv)
criaMegaArv [] arv = arv



main = do entrada <- openFile "entrada.txt" ReadMode
          content <- hGetContents entrada
          let linesOfFile = lines content
          let listas = stringsToInts linesOfFile
          let arvMega = criaMegaArv (criaArvs listas) Nil
          putStr (show arvMega)
          putStr "\n"
          hClose entrada
          
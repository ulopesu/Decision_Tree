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


--SE INSERE NÃO ACHAR, NÃO INSERE
insereN x arv Nil = arv
insereN x (No y fs) (No z gs) | x == z = No z ((No y fs):gs)
                              | otherwise = insereNF x (No y fs) gs
                            
insereNF x arv [] = Nil
insereNF x (No y fs) (z:zs) | (buscaN x z) /= Nil = insereN (No y fs) z
                            | otherwise = insereNF x (No y fs) zs



                            
stringToInt xs = [read x :: Int | x<-xs]
stringsToInts xs = [(stringToInt (words y)) | y <- xs]

criaArvs xxs = reverse [criaArvN xs | xs<-xxs]

-- LISTA (xs:xxs) É UMA LISTA DE ARVORES
criaMegaArv (xs:xxs) Nil y = criaMegaArv xxs xs y
criaMegaArv (xs:xxs) arv y = criaMegaArv xxs (insereN (pegaRaizArvN xs) xs arv)
criaMegaArv [] arv y = arv



main = do entrada <- openFile "entrada.txt" ReadMode
          content <- hGetContents entrada
          let linesOfFile = lines content
          let listas = stringsToInts linesOfFile
          let arvMega = criaMegaArv (criaArvs listas) Nil -1
          putStr (show (criaArvs listas))
          hClose entrada
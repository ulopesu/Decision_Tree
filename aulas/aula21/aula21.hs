import System.IO
--Aula 19

--Arvore N
data Arvn a = Nil
            | No a [Arvn a]
            deriving (Show, Eq)

criaArvN [] = Nil
criaArvN (x:xs) = No x (criaArvNF xs)

criaArvNF [x] = [No x [Nil]]
criaArvNF (x:xs) = [No x [Nil]] ++ criaArvNF xs

{-
insereArv arv Nil = arv
insereArv (No ae0 x ad0) (No ae y ad) | y == x = (No ae0 x ad0)
                                      | y < x = No ae y (insereArv (No ae0 x ad0) ad)
                                      | y > x = No (insereArv (No ae0 x ad0) ae) y ad
-}

stringToInt xs = [read x :: Int | x<-xs]
stringsToInts xs = [(stringToInt (words y)) | y <- xs]

criaArvs xxs = [criaArvN(xs) | xs<-xxs]

criaMegaArv (xs:xxs) = criaMegaArvAux  xxs xs

criaMegaArvAux [x] arv = (insereArv x arv)

criaMegaArvAux (xs:xxs) arv = criaMegaArvAux xxs (insereArv xs arv)


main = do entrada <- openFile "entrada.txt" ReadMode
          content <- hGetContents entrada
          let linesOfFile = lines content
          let listas = stringsToInts linesOfFile
          let arvMega = criaMegaArv (criaArvs listas)
          putStr (show (criaArvs listas))
          hClose entrada
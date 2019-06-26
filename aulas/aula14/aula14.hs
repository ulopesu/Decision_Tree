import System.IO

main :: IO()

{-
main = do putStrLn "Digite uma palavra: "
          palavra <- getLine
         putStrLn "A palavra ao contrario e: "
          putStrLn (show (reverse palavra))
-}

{-
somar0 a b 0 = a                                           AINDA N FUNCIONA
somar0 a b c = somar0 (a+b) (escolha<-getLine) (c-1)

menuSomar = do putStrLn "Digite quantos numeros deseja somar"
               qtd <- getLine
               somar0 0 0 (qtd+1)
-}

{-
menu n | n=="0" = putStrLn "Sair"
       | n=="1" = putStrLn "Somar"
       | n=="2" = putStrLn "Subtrair"
       | n=="3" = putStrLn "Multiplicar"
       | otherwise = putStrLn "Inválido!!!"

main = do putStrLn ""
          putStrLn "Digite uma opção: "
          putStrLn "0)Sair"
          putStrLn "1)Somar"
          putStrLn "2)Subtrair"
          putStrLn "3)Multiplicar"
          escolha <- getLine
          menu escolha
          if escolha /= "0" then main else return()
-}

{-
-- convInt - Converte uma lista de string e uma lista de inteiros
convInt :: [String] -> [Int]
--convInt xs = [y | x<-xs, y<-(read x :: Int)] NAO FUNCIONOU
convInt [] = []
convInt  (x:xs) = (read x :: Int):convInt xs

multiplix [] = 1
multiplix (x:xs) = x * multiplix xs

main = do numeros <- getLine
          putStrLn (show (multiplix (convInt(words numeros))))
-}
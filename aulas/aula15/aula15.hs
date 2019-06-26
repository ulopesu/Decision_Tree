import System.IO

main :: IO()
main = do entrada <- openFile "notas.txt" ReadMode
 	  saida <- openFile "saida.txt" WriteMode
	  texto <- readFile entrada
	  linhas <- lines texto
	  putStr linhas
	  -- writeFile "saida.txt" saida


media y = show ((sum (map read (pals))) / fromIntegral (length pals))
 where
  pals = words y

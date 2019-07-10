module Results where
import DecisionTree
import Value
{-
generateResults :: DecTree -> [[[Char]]] -> [Char]
generateResults arv [] = []
generateResults arv (c:cs) = newResult ++ nextResult
    where newResult = generateResult arv c
          nextResult = generateResults arv cs

generateResult _ [] = "Sem casos para testar!"
generateResult (Leaf result) cas = result ++ "\n" 
generateResult (Tree (nameF, values, idF, kind)) cas = result ++ "\n"
    where result = calcResult values valCase kind
          valCase = cas!!idF

          
calcResult :: [Value] -> [Char] -> [Char] -> [Char]
calcResult values kind [] = []
calcResult values [] kind = []        
calcResult [] valCase kind = []
calcResult values valCase kind | kind == "int" = calcIntResult values (read valCase :: Float)
                               | otherwise = calcStrResult values valCase

calcStrResult :: [Value] -> [Char] -> [Char]
calcStrResult [] valCase = "Sem solucao para este caso: " ++ valCase
calcStrResult (v:vs) valCase | getStrV v == valCase = head (getDSV v)
                             | otherwise = calcStrResult vs valCase

calcIntResult :: [Value] -> Float -> [Char]
calcIntResult [] valCase = "Sem solucao para este caso: " ++ (show valCase)
calcIntResult (v:vs) valCase | (valCase > getID0V v) && (valCase <= getID1V v) = head $ getDSV v
                             | (getID0V v == 0) && (valCase <= getID1V v) = head $ getDSV v
                             | (getID0V v == getID1V v) && (valCase > getID0V v) = head $ getDSV v
                             | otherwise = calcIntResult vs valCase

                             -}
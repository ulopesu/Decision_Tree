module Output where
import DecisionTree
import Value

generateResultSTR :: DecTree -> [[String]] -> String
generateResultSTR arv [] = []
generateResultSTR arv (c:cs) = newResult ++"\n"++ nextResult
    where newResult = generateResult arv c
          nextResult = generateResultSTR arv cs

generateResult :: DecTree -> [String] -> String
generateResult _ [] = "Sem casos para testar!\n"
generateResult (Leaf result) cas = result
generateResult (Tree (nameF, idF, valuesAndTrees)) cas = result
    where result = calcResult valuesAndTrees cas idF


calcResult:: [(Value, DecTree)] -> [String] -> Int -> String
calcResult [] cas idF = []
calcResult (vt:vts) [] idF = []
calcResult (vt:vts) cas idF | calcVT /= [] = calcVT | otherwise = calcVTS
    where calcVT = calcResultVT vt cas idF
          calcVTS = calcResult vts cas idF
    

calcResultVT:: (Value, DecTree) -> [String] -> Int -> String
calcResultVT (value, tree) cas idF | (result /= "nextTree") = result | (result == "nextTree") && (not isLeaf) = generateResult tree cas | otherwise = []
    where result = calcResultValue value valCase
          isLeaf = isLeafTree tree
          valCase = cas!!idF

calcResultValue (ValueStr (vName, decisions)) valCase | (length decisions) > 1 = "nextTree" 
                                                      | vName == valCase = head decisions
                                                      | otherwise = []

calcResultValue (ValueInt (id0, id1, decisions)) value | (length decisions) > 1 = "nextTree"
                                                       | (valCase > id0) && (valCase <= id1) = head decisions
                                                       | (id0 == 0) && (valCase <= id1) = head decisions
                                                       | (id0 == id1) && (valCase > id0) = head decisions
                                                       | otherwise = []
    where valCase = read value :: Float




--data DecTree = Leaf String | Tree (String, Int, [(Value, DecTree)]) deriving (Show)
--ValueStr (String, [String]) | ValueInt (Float, Float, [String]) 


generateArvSTR (Leaf decision) tab = []
generateArvSTR (Tree (nameF, idF, valuesAndTrees)) tab = generateArvStrVTS nameF valuesAndTrees tab

generateArvStrVTS nameF [] tab = []
generateArvStrVTS nameF (vt:vts) tab | (next /= []) && (new /= []) = saida0 | (new /= []) = saida1 | otherwise = []
    where new =  generateArvStrVT nameF vt tab
          next = generateArvStrVTS nameF vts (tab++"\t")
          saida0 = new ++ tab ++ "senao\n" ++ next
          saida1 = new ++ "fim-se\n" 


generateArvStrVT nameF (value, tree) tab = genArvStrV ++ nexTree
    where genArvStrV = generateArvStrValue value nameF tab
          nexTree = generateArvSTR tree tab


generateArvStrValue (ValueStr (vName, decisions)) nameF tab | lenDe > 1 = [] | otherwise = saida
    where lenDe = length decisions
          decision = head decisions
          saida = tab ++ "se " ++ nameF ++ " = "++ vName ++ " entao\n" ++ tab ++"\tretorne " ++ decision ++ "\n" ++ tab ++ "fim-se\n"


generateArvStrValue (ValueInt (id0, id1, decisions)) nameF tab | lenDe > 1 = [] 
                                                               | id0 == 0 = saida0
                                                               | id0 == id1 = saida1
                                                               | otherwise = saida2
    where lenDe = length decisions
          decision = head decisions
          saida0 = tab ++ "se " ++ nameF ++ " <= "++ (show id1) ++ " entao\n" ++ tab ++ "\tretorne " ++ decision ++"\n" ++ tab ++ "fim-se\n"
          saida1 = tab ++ "se " ++ nameF ++ " > "++ (show id0) ++ " entao\n" ++ tab ++ "\tretorne " ++ decision ++"\n" ++ tab ++ "fim-se\n"
          saida2 = tab ++ "se " ++ (show id0) ++ " > " ++ nameF ++ " <= " ++ (show id1)  ++ " entao\n" ++ tab ++ "\tretorne " ++ decision ++ "\n" ++ tab ++ "fim-se\n"



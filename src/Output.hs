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
generateArvStrVTS nameF (vt:vts) tab | (next /= []) = saida0 | otherwise = new ++ tab ++ "fim-se\n"
    where new =  generateArvStrVT nameF vt tab
          next = generateArvStrVTS nameF vts (tab++"   ")
          saida0 = new ++ tab ++ "senao\n" ++ next ++ tab ++ "fim-se\n"


generateArvStrVT nameF (value, tree) tab | (not leafTree) = genArvStrV ++ nexTree | otherwise = genArvStrV 
    where genArvStrV = generateArvStrValue value nameF tab
          nexTree = generateArvSTR tree (tab++"   ")
          leafTree = isLeafTree tree


generateArvStrValue (ValueStr (vName, decisions)) nameF tab | lenDe > 1 = saida1 | otherwise = saida0
    where lenDe = length decisions
          decision = head decisions
          saida0 = tab ++ "se " ++ nameF ++ " = "++ vName ++ " entao\n" ++ tab ++"   retorne " ++ decision ++ "\n"
          saida1 = tab ++ "se " ++ nameF ++ " = "++ vName ++ " entao\n" 


generateArvStrValue (ValueInt (id0, id1, decisions)) nameF tab | (lenDe > 1) && (id0 == 0) = saida0
                                                               | (lenDe > 1) && (id0 == id1) = saida1
                                                               | (lenDe > 1) = saida2
                                                               | id0 == 0 = saida3
                                                               | id0 == id1 = saida4
                                                               | otherwise = saida5
    where lenDe = length decisions
          decision = head decisions
          saida0 = tab ++ "se " ++ nameF ++ " <= "++ (show id1) ++ " entao\n" 
          saida1 = tab ++ "se " ++ nameF ++ " > "++ (show id0) ++ " entao\n"
          saida2 = tab ++ "se " ++ (show id0) ++ " < " ++ nameF ++ " <= " ++ (show id1)  ++ " entao\n"
          saida3 = tab ++ "se " ++ nameF ++ " <= "++ (show id1) ++ " entao\n" ++ tab ++ "   retorne " ++ decision ++"\n"
          saida4 = tab ++ "se " ++ nameF ++ " > "++ (show id0) ++ " entao\n" ++ tab ++ "   retorne " ++ decision ++"\n"
          saida5 = tab ++ "se " ++ (show id0) ++ " < " ++ nameF ++ " <= " ++ (show id1)  ++ " entao\n" ++ tab ++ "   retorne " ++ decision ++ "\n"





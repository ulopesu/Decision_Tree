module DecisionTree where
import Data.List
import Data.Ord


getDescription:: ([[String]], [[String]], [[String]], [[Char]]) -> [[String]]
getDescription (description, _, _, _) = description

getBase:: ([[String]], [[String]], [[String]], [[Char]]) -> [[String]]
getBase (_, base, _, _) = base

getCases:: ([[String]], [[String]], [[String]], [[Char]]) -> [[String]]
getCases (_, _, cases, _) = cases

getTypes:: ([[String]], [[String]], [[String]], [String]) -> [String]
getTypes (_, _, _, types) = types


data Feature = Feature (String, [Value], String) deriving (Show)

data Value  = ValueStr (String, [String]) | ValueInt (Int, Int, [String]) deriving (Show, Eq)

--getDecisionsValue (getDSV)
getDSV:: Value -> [String]
getDSV (ValueStr (_, x)) = x
getDSV (ValueInt (_, _, x)) = x

--getIntValue (getISV)
getISV:: Value -> Int
getISV (ValueInt (x, _, _)) = x

--getStringValue (getStrV)
getStrV:: Value -> String
getStrV (ValueStr (x, _)) = x

--addStringOnValue (addStrOnV)
addStrOnV:: Value -> String -> Value
addStrOnV (ValueInt (x, y, z)) str = ValueInt (x, y, z++[str])
addStrOnV (ValueStr (x, y)) str = ValueStr (x, y++[str])


instance Ord Value where
  ValueInt (x, _, _) < ValueInt (y, _, _) = x < y
  ValueInt (x, _, _) <= ValueInt (y, _, _) = x <= y
  ValueInt (x, _, _) >= ValueInt (y, _, _) = x >= y
  ValueInt (x, _, _) > ValueInt (y, _, _) = x > y

createFeatures :: [[String]] -> [[String]] -> [String] -> [Feature]
createFeatures headerFeatures base types = createFeaturesAux headerFeatures base types 0

createFeaturesAux :: [[String]] -> [[String]] -> [String] -> Int -> [Feature]
createFeaturesAux [] base types pos = []
createFeaturesAux (xs:xss) base types pos = newF:nextF
   where newF = Feature (head xs, getValuesBase (tail xs) base pos, types!!pos)
         nextF = createFeaturesAux xss base types (pos+1)         

getValuesBase :: [String] -> [[String]] -> Int -> [Value]
getValuesBase [] base pos = newVSInt
  where newVSInt = getExamplesInt base pos

getValuesBase [v]  base pos = [newVStr]
  where newVStr =  ValueStr (v, getExamplesStr v base pos)

getValuesBase (v:vs) base pos = newVStr:nextVStr
  where newVStr =  ValueStr (v, getExamplesStr v base pos)
        nextVStr = getValuesBase vs base pos

getExamplesStr v [] pos = []
getExamplesStr v (b:bs) pos | v == (b!!pos) = (last b):(getExamplesStr v bs pos)
                            | otherwise = getExamplesStr v bs pos

getExamplesInt base pos = filterVSInt (sort (generateExamples base pos))
-- FAZER AQUI A LIMPEZA DOS EXEMPLOS ENUMERADOS, NAO PRECISO DELES PARA CALCULAR A ENTROPIA


generateExamples::  [[String]] -> Int -> [Value]

generateExamples [] _ = []
generateExamples (b:bs) pos = newB:nextB
  where newB = ValueInt (intEx, intEx, [last b])
        nextB = generateExamples bs pos
        intEx = read (b!!pos) ::Int



filterVSInt :: [Value] -> [Value]
filterVSInt examples = filterVSIntAux examples (ValueInt (0, 0, [])) 0 0 []

-- PRIMEIRO CASO
filterVSIntAux (e:es) initValue len intAnt [] = filterVSIntAux es newValueInt (len+1) intAtual strAtual
  where strAtual = head (getDSV e)
        intAtual = getISV e
        newValueInt = addStrOnV initValue strAtual

-- INTERMEDIARIO
filterVSIntAux (e:es) (ValueInt (id1, id2, stgs)) len intAnt strAnt | strAtual == strAnt = filterVSIntAux es (addStrOnV (ValueInt (id1, id2, stgs)) strAtual) (len+1) (getISV e) (head (getDSV e))
                                                                    | otherwise = [(ValueInt (vId1, mediana, stgs))] ++ (filterVSIntAux es newValueInt (len+1) intAtual strAtual)
  where mediana = (intAnt + intAtual) `div` 2
        intAtual = getISV e
        strAtual = head (getDSV e)
        newValueInt = ValueInt (mediana, 0, [strAtual])
        vId1 = calcId1 id1 mediana

-- ULTIMO CASO
filterVSIntAux [] (ValueInt (id1, id2, stgs)) len intAnt strAnt = [ValueInt (intAnt, intAnt, stgs)]

                                                                              
calcId1 0 med = med
calcId1 x _ = x




--ValueInt (Int, Int, [String])




 --PSEUDO-CODIGO
{-
arvoreDecisao (exemplos, caracteristicas, maisComum): arvore
  se (exemplos é vazio) entao retorne maisComum;
  senão se (todos os exemplos têm a mesma classificação) entao retorne (a classificação);
  senão se (não há mais características) então retorne maioria(exemplos);
  senão
    melhor <- melhorTeste(características,exemplos);
    árvore <- nova árvore com raiz “melhor”;

    para cada valor vi de melhor faça
      exemplosi <- exemplos onde melhor = vi;
      subárvore <- arvoreDecisao(exemplosi,
        caracteristicas-{melhor}, maioria(exemplos));
      adicione subárvore como um ramo à árvore com
        rótulo vi;

  retorne arvore;
-}

--data DecisionTree = String | Feature


          
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

data Value  = ValueStr (String, [String]) | ValueInt (Float, Float, [String]) deriving (Show, Eq)

--getDecisionsValue (getDSV)
getDSV:: Value -> [String]
getDSV (ValueStr (_, x)) = x
getDSV (ValueInt (_, _, x)) = x

--getIntValue (getISV)
getISV:: Value -> Float
getISV (ValueInt (x, _, _)) = x

--getStringValue (getStrV)
getStrV:: Value -> String
getStrV (ValueStr (x, _)) = x

--addStringOnValue (addStrOnV)
addStrOnV:: Value -> String -> Value
addStrOnV (ValueInt (x, y, z)) str = ValueInt (x, y, z++[str])
addStrOnV (ValueStr (x, y)) str = ValueStr (x, y++[str])


instance Ord Value where
  ValueInt (_, x, _) < ValueInt (_, y, _) = x < y
  ValueInt (_, x, _) <= ValueInt (_, y, _) = x <= y
  ValueInt (_, x, _) >= ValueInt (_, y, _) = x >= y
  ValueInt (_, x, _) > ValueInt (_, y, _) = x > y


-- getDecisionsFeature (gDF)
gDF:: Feature -> [String]
gDF (Feature (nameF, values, kind)) = gDVS values

-- getDecisionsValues gDVS
gDVS [] = []
gDVS (v:vs) = (gDV v)++ gDVS vs

-- getDecisionsValue gDV
gDV:: Value -> [String]
gDV (ValueStr (value, examples)) = examples
gDV (ValueInt (value, value1, examples)) = examples

qtdExamples:: Feature -> Int
qtdExamples (Feature (nameF, values, kind)) = qtdExValues values

qtdExValues:: [Value] -> Int
qtdExValues [] = 0
qtdExValues (v:vs) = (qtdExValue v) + (qtdExValues vs) 

qtdExValue:: Value -> Int
qtdExValue (ValueStr (value, examples)) = length examples
qtdExValue (ValueInt (value, value1, examples)) = length examples


getExFeatures :: [Feature] -> [String]
getExFeatures [] = []
getExFeatures (x:xs) = (gDF x) ++ (getExFeatures xs)


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


generateExamples::  [[String]] -> Int -> [Value]
generateExamples [] _ = []
generateExamples (b:bs) pos = newB:nextB
  where newB = ValueInt (intEx, intEx, [last b])
        nextB = generateExamples bs pos
        intEx = read (b!!pos) ::Float



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
  where mediana = (intAnt + intAtual) / 2
        intAtual = getISV e
        strAtual = head (getDSV e)
        newValueInt = ValueInt (mediana, 0, [strAtual])
        vId1 = calcId1 id1 mediana

-- ULTIMO CASO
filterVSIntAux [] (ValueInt (id1, id2, stgs)) len intAnt strAnt = [ValueInt (id1, id1, stgs)]
                                                                     
calcId1 0 med = 0
calcId1 x _ = x


                   

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
{-

data ArvDec =  String | Value | No (Feature, [ArvDec])

--arvoreDecisao (arvDec)
arvDec [] features mostCommon = mostCommon
arvDec examples features mostCommon | sameClass examples = classExamples (examples)
                                    | feature == [] = maioria examples
                                    | otherwise = createRoot
          -}
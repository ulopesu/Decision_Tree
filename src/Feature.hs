module Feature where
import Data.List
import Value

data Feature = Feature (String, [Value], String) deriving (Show)

qtdExamples:: Feature -> Int
qtdExamples (Feature (nameF, values, kind)) = qtdExValues values

getExFeatures :: [Feature] -> [String]
getExFeatures [] = []
getExFeatures (x:xs) = (gDF x) ++ (getExFeatures xs)

-- getDecisionsFeature (gDF)
gDF:: Feature -> [String]
gDF (Feature (nameF, values, kind)) = gDVS values


createFeatures :: [[String]] -> [[String]] -> [String] -> [Feature]
createFeatures headerFeatures base types = createFeaturesAux headerFeatures base types 0

createFeaturesAux :: [[String]] -> [[String]] -> [String] -> Int -> [Feature]
createFeaturesAux [] base types pos = []
createFeaturesAux (xs:xss) base types pos = newF:nextF
   where newF = Feature (head xs, values, types!!pos)
         nextF = createFeaturesAux xss base types (pos+1)    
         values = getValuesBase (tail xs) base pos


getValuesBase :: [String] -> [[String]] -> Int -> [Value]
getValuesBase [] base pos = newVSInt
  where newVSInt = getExamplesInt base pos

getValuesBase [v]  base pos = [newVStr]
  where newVStr =  ValueStr (v, getExamplesStr v base pos)

getValuesBase (v:vs) base pos = newVStr:nextVStr
  where newVStr =  ValueStr (v, getExamplesStr v base pos)
        nextVStr = getValuesBase vs base pos


getExamplesStr :: Eq a => a -> [[a]] -> Int -> [a]
getExamplesStr v [] pos = []
getExamplesStr v (b:bs) pos | v == (b!!pos) = (last b):(getExamplesStr v bs pos)
                            | otherwise = getExamplesStr v bs pos

getExamplesInt :: [[String]] -> Int -> [Value]
getExamplesInt base pos = filterVSInt (sort (generateExamples base pos))

generateExamples::  [[String]] -> Int -> [Value]
generateExamples [] _ = []
generateExamples (b:bs) pos = newB:nextB
  where newB = ValueInt (intEx, intEx, [last b])
        nextB = generateExamples bs pos
        intEx = read (b!!pos) ::Float


filterVSInt :: [Value] -> [Value]
filterVSInt examples = filterVSIntAux examples (ValueInt (0, 0, [])) 0 0 []

filterVSIntAux :: Num a => [Value] -> Value -> a -> Float -> String -> [Value]
filterVSIntAux (e:es) initValue len intAnt [] = filterVSIntAux es newValueInt (len+1) intAtual strAtual
  where strAtual = head (getDSV e)
        intAtual = getID0V e
        newValueInt = addStrOnV initValue strAtual

filterVSIntAux [] (ValueInt (id1, id2, stgs)) len intAnt strAnt = [ValueInt (id1, id1, stgs)]
filterVSIntAux (e:es) (ValueInt (id1, id2, stgs)) len intAnt strAnt | strAtual == strAnt = filterVSIntAux es (addStrOnV (ValueInt (id1, id2, stgs)) strAtual) (len+1) (getID0V e) (head (getDSV e))
                                                                    | otherwise = [(ValueInt (vId1, mediana, stgs))] ++ (filterVSIntAux es newValueInt (len+1) intAtual strAtual)
  where mediana = (intAnt + intAtual) / 2
        intAtual = getID0V e
        strAtual = head (getDSV e)
        newValueInt = ValueInt (mediana, 0, [strAtual])
        vId1 = calcId1 id1 mediana

calcId1 :: (Eq a, Num a, Num p) => a -> p -> p                                      
calcId1 0 med = 0
calcId1 _ med = med
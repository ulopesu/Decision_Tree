module DecisionTree where
import Data.List
import Feature
import Value
import InformationGain


--ESTUDAR DESEMPATE PARA ValueSTR

data ArvDec = Leaf String | Val Value | Root (String, Int, [ArvDec]) deriving (Show)

--arvoreDecisao (arvDec)
newArvDec:: [Feature] -> ArvDec
newArvDec [] = Leaf []
newArvDec features | sameClass examples = arvStr | otherwise = createRoot feature indexBest
  where feature = features!!(indexBest)
        examples = gDF feature
        arvStr = Leaf (theClassEx examples)
        indexBest = bestIGR features

createRoot:: Feature -> Int -> ArvDec
createRoot (Feature (nameF, values, kind)) idBest = Root (nameF, idBest, map Val values)

theClassEx :: [String] -> String
theClassEx [] = []
theClassEx (x:xs) = x
                                    
sameClass :: [String] -> Bool
sameClass strings | gLength == 1 = True | otherwise = False
  where groups = group $ sort strings
        gLength = length groups


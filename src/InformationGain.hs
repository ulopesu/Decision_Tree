module InformationGain where
import Data.List

import DecisionTree


entropy :: (Ord a, Floating b) => [a] -> b
entropy = sum . map lg . fq . map genericLength . group . sort 
  where lg b = -b * logBase 2 b
        fq b = let sb = sum b in map (/ sb) b




--data Value  = ValueStr (String, [String]) | ValueInt (Int, String) deriving (Show)

--data Feature = Feature (String, [Value], String) deriving (Show)

--biggerInformationGain

bIG:: [Feature] -> Int

bIG features = biggerID (iGFeatures features)

biggerID :: [Float] -> Int
biggerID xs =  biggerIDAux xs 0 0

biggerIDAux :: [Float] -> Int -> Int -> Int
biggerIDAux [] _ _ = -1
biggerIDAux [x] idMaior idAtual = idMaior
biggerIDAux (x:xs) idMaior idAtual | (head xs) > x = biggerIDAux xs (idAtual+1) (idAtual+1)
                                   | otherwise = biggerIDAux xs idMaior (idAtual+1)

iGFeatures:: [Feature] -> [Float]
iGFeatures [] = []
iGFeatures (f:fs) = [(iGainR (infoRoot f) f)]++(iGFeatures fs)

--informationGainRaise (iGainR)
iGainR :: (Float, Int) -> Feature -> Float
iGainR (entFeature, qtdExamples) (Feature (nameF, values, kind)) = (iGain entFeature values qtdExamples) / (sumVIValues values qtdExamples)*(-1)


infoRoot :: Feature -> (Float, Int)
infoRoot feature = (entropyFeature feature, qtdExamples feature)

entropyFeature feature = entropy $ gDF feature

--informationGain
iGain entFeature values qtdExamples = entFeature - (sumEntValues values qtdExamples)


sumEntValues [] _ = 0
sumEntValues (v:vs) qtdExamples = (sumEntValue v qtdExamples) + (sumEntValues vs qtdExamples)


sumEntValue :: Value -> Int -> Float
sumEntValue (ValueStr (value, examples)) qtdExamples =  (fromIntegral (length examples) / fromIntegral qtdExamples) * (entropy examples)
sumEntValue (ValueInt (value, value1, examples)) qtdExamples =  (fromIntegral (length examples) / fromIntegral qtdExamples) * (entropy examples)


-- Valor Intrínseco
sumVIValues [] _ = 0
sumVIValues (v:vs) qtdExamples = (sumVIValue v qtdExamples) + (sumVIValues vs qtdExamples)


sumVIValue :: Value -> Int -> Float
sumVIValue (ValueStr (value, examples)) qtdExamples = fract * (logBase 2 fract)
  where fract = fromIntegral (length examples) / fromIntegral qtdExamples

sumVIValue (ValueInt (value, value1, examples)) qtdExamples = fract * (logBase 2 fract)
  where fract = fromIntegral (length examples) / fromIntegral qtdExamples



mostC :: [String] -> String
mostC strings = head (groups!!idMostCommon)
  where groups = group $ sort strings
        idMostCommon = biggerID (map fromIntegral (map length (groups)))


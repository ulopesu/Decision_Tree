module InformationGain where
import Data.List
import Feature
import Value

entropy :: (Ord a, Floating b) => [a] -> b
entropy = sum . map lg . fq . map genericLength . group . sort 
  where lg b = -b * logBase 2 b
        fq b = let sb = sum b in map (/ sb) b


--biggerInformationGain
bestIGR:: [Feature] -> Int
bestIGR features = biggerID (iGFeatures features)


iGFeatures:: [Feature] -> [Float]
iGFeatures [] = []
iGFeatures (f:fs) = [(iGainR (infoRoot f) f)]++(iGFeatures fs)

--informationGainRaise (iGainR)
iGainR :: (Float, Int) -> Feature -> Float
iGainR (entFeature, qtdExamples) (Feature (nameF, values, kind)) = (iGain entFeature values qtdExamples) / (sumVIValues values qtdExamples)*(-1)


infoRoot :: Feature -> (Float, Int)
infoRoot feature = (entropyFeature feature, qtdExamples feature)

entropyFeature :: Floating b => Feature -> b
entropyFeature feature = entropy $ gDF feature

--informationGain
iGain :: Float -> [Value] -> Int -> Float
iGain entFeature values qtdExamples = entFeature - (sumEntValues values qtdExamples)

sumEntValues :: [Value] -> Int -> Float
sumEntValues [] _ = 0
sumEntValues (v:vs) qtdExamples = (sumEntValue v qtdExamples) + (sumEntValues vs qtdExamples)

sumEntValue :: Value -> Int -> Float
sumEntValue (ValueStr (value, examples)) qtdExamples =  (fromIntegral (length examples) / fromIntegral qtdExamples) * (entropy examples)
sumEntValue (ValueInt (value, value1, examples)) qtdExamples =  (fromIntegral (length examples) / fromIntegral qtdExamples) * (entropy examples)


-- Valor Intrínseco
sumVIValues :: [Value] -> Int -> Float
sumVIValues [] _ = 0
sumVIValues (v:vs) qtdExamples = (sumVIValue v qtdExamples) + (sumVIValues vs qtdExamples)


sumVIValue :: Value -> Int -> Float
sumVIValue (ValueStr (value, examples)) qtdExamples = fract * (logBase 2 fract)
  where fract = fromIntegral (length examples) / fromIntegral qtdExamples

sumVIValue (ValueInt (value, value1, examples)) qtdExamples = fract * (logBase 2 fract)
  where fract = fromIntegral (length examples) / fromIntegral qtdExamples



        


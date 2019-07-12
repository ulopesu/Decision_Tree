module InformationGain where
import Data.List
import Feature
import Value

entropy :: (Ord a, Floating c) => [a] -> c  
entropy = sum . map lg . fq . map genericLength . group . sort 
  where lg c = -c * logBase 2 c
        fq c = let sc = sum c in map (/ sc) c



--biggerInformationGain
bestIGR:: [Feature] -> Int
bestIGR features = biggerID (iGFeatures features (qtdExpFS features)) 

iGFeatures:: [Feature] -> Int -> [Float]
iGFeatures [] qtdExps = []
iGFeatures (f:fs) qtdExps = [iGainR entropyF qtdExps f]++(iGFeatures fs qtdExps)
  where entropyF = entropyFeature f

--informationGainRaise (iGainR)
iGainR :: Float -> Int -> Feature -> Float
iGainR entFeature qtdExamples (Feature (nameF, values)) = (iGain entFeature values qtdExamples) / (sumVIValues values qtdExamples)*(-1)


entropyFeature :: Floating b => Feature -> b
entropyFeature feature = entropy $ gDF feature

--informationGain
iGain :: Float -> [Value] -> Int -> Float
iGain entFeature values qtdExamples = entFeature - (sumEntValues values qtdExamples)

sumEntValues :: [Value] -> Int -> Float
sumEntValues [] _ = 0
sumEntValues (v:vs) qtdExamples = (sumEntValue v qtdExamples) + (sumEntValues vs qtdExamples)

sumEntValue :: Value -> Int -> Float
sumEntValue (ValueStr (value, examples)) qtdExamples = (fromIntegral (length examples) / fromIntegral qtdExamples) * (entropy examples)
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



        


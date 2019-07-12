module Value where
import Data.List

data Value  = ValueStr (String, [String]) | ValueInt (Float, Float, [String]) deriving (Show, Eq)

instance Ord Value where
    ValueInt (_, x, _) < ValueInt (_, y, _) = x < y
    ValueInt (_, x, _) <= ValueInt (_, y, _) = x <= y
    ValueInt (_, x, _) >= ValueInt (_, y, _) = x >= y
    ValueInt (_, x, _) > ValueInt (_, y, _) = x > y

--getDecisionsValue (getDSV)
getDSV:: Value -> [String]
getDSV (ValueStr (_, x)) = x
getDSV (ValueInt (_, _, x)) = x

--getID0Value (getISV)
getID0V:: Value -> Float
getID0V (ValueInt (x, _, _)) = x

--getIntValue (getISV)
getID1V:: Value -> Float
getID1V (ValueInt (_, x, _)) = x

--getStringValue (getStrV)
getStrV:: Value -> String
getStrV (ValueStr (x, _)) = x



--addStringOnValue (addStrOnV)
addStrOnV:: Value -> String -> Value
addStrOnV (ValueInt (x, y, z)) str = ValueInt (x, y, z++[str])
addStrOnV (ValueStr (x, y)) str = ValueStr (x, y++[str])
  
qtdExValues:: [Value] -> Int
qtdExValues [] = 0
qtdExValues (v:vs) = (qtdExValue v) + (qtdExValues vs) 

qtdExValue:: Value -> Int
qtdExValue (ValueStr (value, examples)) = length examples
qtdExValue (ValueInt (value, value1, examples)) = length examples

-- getDecisionsValues gDVS
gDVS :: [Value] -> [String]
gDVS [] = []
gDVS (v:vs) = (getDSV v)++ gDVS vs

cleanValue:: Value -> Value
cleanValue (ValueStr (name, decisions)) =  ValueStr (name, newD)
  where newD = map head (group $ sort decisions)
cleanValue (ValueInt (v0, v1, decisions)) =  ValueInt (v0, v1, newD)
  where newD = map head (group $ sort decisions)

mostC :: [String] -> String
mostC [] = []
mostC strings = head (groups!!idMostCommon)
  where groups = group $ sort strings
        idMostCommon = biggerID (map fromIntegral (map length (groups)))

biggerID :: [Float] -> Int
biggerID xs =  biggerIDAux xs 0 0

biggerIDAux :: [Float] -> Int -> Int -> Int
biggerIDAux [] _ _ = -1
biggerIDAux [x] idMaior idAtual = idMaior
biggerIDAux (x:xs) idMaior idAtual | (head xs) > x = biggerIDAux xs (idAtual+1) (idAtual+1)
                                   | otherwise = biggerIDAux xs idMaior (idAtual+1)
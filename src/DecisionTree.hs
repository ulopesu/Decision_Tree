module DecisionTree where
import Data.List
import Feature
import Value
import InformationGain
                           --(nameFeature, [(nameValue, nextTree)], id)
data DecTree = Leaf String | Tree (String, [(Value, DecTree)], Int) deriving (Show)

newDecTree:: [Feature] -> [[String]] -> [[String]] -> String -> DecTree
newDecTree features headerFS [] mostCommon = Leaf mostCommon
newDecTree [] headerFS base mostCommon = Leaf (mostC (getClassesBase base))
newDecTree features headerFS base mostCommon | sameClass = Leaf (last $ head base) | otherwise = createTree features headerFS base
  where classesBase = getClassesBase base
        sameClass = isSameClass classesBase                             
        
createTree:: [Feature] -> [[String]] -> [[String]] -> DecTree         
createTree features headerFS base = (Tree (nameF, nextTrees, idBest))
  where idBest = bestIGR features
        bestF = features!!idBest
        nameF = gNF bestF
        values = gVSF bestF
        nextTrees = createTreesValues idBest values (cleanList headerFS idBest) base

      
createTreesValues:: Int -> [Value] -> [[String]] -> [[String]] -> [(Value, DecTree)]                               
createTreesValues idBest [] newHeader base = []
createTreesValues idBest (v:vs) newHeader base = [point] ++ nextTrees
  where newB = cleanBase (newBase idBest v base) idBest
        newFeatures = createFeatures newHeader newB
        mostCommon = mostC $ getClassesBase newB
        newTree = newDecTree newFeatures newHeader newB mostCommon
        point = (cleanValue v, newTree)
        nextTrees = createTreesValues idBest vs newHeader base


newBase :: Int -> Value -> [[String]] -> [[String]]
newBase idBest value [] = []
newBase idBest (ValueStr (nameV, exs)) (b:bs) | nameV == (b!!idBest) = b:nextB | otherwise = nextB
  where nextB = newBase idBest (ValueStr (nameV, exs)) bs

newBase idBest (ValueInt (id0, id1, exs)) (b:bs) | (idX > id0) && (idX <= id1) = b:nextB | (id0 == 0) && (idX < id1) = b:nextB | (id0 == id1) && (idX > id1) = b:nextB | otherwise = nextB
  where idX = read (b!!idBest) :: Float
        nextB = newBase idBest (ValueInt (id0, id1, exs)) bs


cleanBase:: [[a]] -> Int -> [[a]]
cleanBase [] id = []   
cleanBase (b:bs) id = [cleanList b id] ++ (cleanBase bs id)
        
cleanList:: [a] -> Int -> [a]
cleanList [] id = []
cleanList list id = (take id list) ++ (drop (id+1) list)

getClassesBase:: [[String]] -> [String]
getClassesBase [] = []
getClassesBase (b:bs) = [(last b)] ++ (getClassesBase bs)
                                    
isSameClass:: [String] -> Bool
isSameClass strings | gLength == 1 = True | otherwise = False
  where groups = group $ sort strings
        gLength = length groups

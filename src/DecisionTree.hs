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

data Value  = ValueStr (String, [String]) | ValueInt (Int, [String]) deriving (Show)

data Feature = Feature (String, [Value], String) deriving (Show)

createFeatures :: [[String]] -> [[String]] -> [String] -> [Feature]
createFeatures headerFeatures base types = createFeaturesAux headerFeatures base types 0

createFeaturesAux :: [[String]] -> [[String]] -> [String] -> Int -> [Feature]
createFeaturesAux [] base types pos = []
createFeaturesAux (xs:xss) base types pos = newF:nextF
   where newF = Feature (head xs, getValuesBase (tail xs) base pos, types!!pos)
         nextF = createFeaturesAux xss base types (pos+1)         

getValuesBase :: [String] -> [[String]] -> Int -> [Value]
getValuesBase [] base pos = newVSInt
  where newVSInt = newValuesInt (getExamplesInt base pos)

getValuesBase [v]  base pos = [newVStr]
  where newVStr =  ValueStr (v, getExamplesStr v base pos)

getValuesBase (v:vs) base pos = newVStr:nextVStr
  where newVStr =  ValueStr (v, getExamplesStr v base pos)
        nextVStr = getValuesBase vs base pos

getExamplesStr v [] pos = []
getExamplesStr v (b:bs) pos | v == (b!!pos) = (last b):(getExamplesStr v bs pos)
                            | otherwise = getExamplesStr v bs pos

getExamplesInt base pos = (sortBy (comparing fst) (generateExamples base pos))
-- FAZER AQUI A LIMPEZA DOS EXEMPLOS ENUMERADOS, NAO PRECISO DELES PARA CALCULAR A ENTROPIA

generateExamples [] _ = []
generateExamples (b:bs) pos = newB:nextB
  where newB = (read (b!!pos) ::Int, last b)
        nextB = generateExamples bs pos

newValuesInt:: [(Int, String)] -> [Value]
newValuesInt [] = []
newValuesInt (e:es) = newVInt:nextVInt
  where newVInt = ValueInt (getIntExample e, getStrExample e)
        nextVInt = newValuesInt es

getIntExample:: (Int, String) -> Int
getIntExample (x,_) = x
getStrExample:: (Int, String) -> [String]
getStrExample (_,y) = [y]





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


          
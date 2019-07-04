import System.IO
import Data.List
import Data.Ord
import Data.Maybe
import Input (readAll)

getDescription:: ([[String]], [[String]], [[String]], [[Char]]) -> [[String]]
getDescription (description, _, _, _) = description

getBase:: ([[String]], [[String]], [[String]], [[Char]]) -> [[String]]
getBase (_, base, _, _) = base

getCases:: ([[String]], [[String]], [[String]], [[Char]]) -> [[String]]
getCases (_, _, cases, _) = cases

getTypes:: ([[String]], [[String]], [[String]], [String]) -> [String]
getTypes (_, _, _, types) = types

data Value  = ValueStr (String, [String]) | ValueInt (Int, String) deriving (Show)

data Feature = Feature (String, [Value], String) deriving (Show)

createFeatures :: [[String]] -> [[String]] -> [String] -> [Feature]
createFeatures headerFeatures base types = createFeaturesAux headerFeatures base types 0

createFeaturesAux :: [[String]] -> [[String]] -> [String] -> Int -> [Feature]
createFeaturesAux [] base types pos = []
createFeaturesAux (xs:xss) base types pos = newF:nextF
   where newF = Feature (head xs, getValues (tail xs) base pos, types!!pos)
         nextF = createFeaturesAux xss base types (pos+1)         

getValues :: [String] -> [[String]] -> Int -> [Value]
getValues [] base pos = newVSInt
  where newVSInt = newValuesInt (getExamplesInt base pos)

getValues [v]  base pos = [newVStr]
  where newVStr =  ValueStr (v, getExamplesStr v base pos)

getValues (v:vs) base pos = newVStr:nextVStr
  where newVStr =  ValueStr (v, getExamplesStr v base pos)
        nextVStr = getValues vs base pos

getExamplesStr v [] pos = []
getExamplesStr v (b:bs) pos | v == (b!!pos) = (last b):(getExamplesStr v bs pos)
                            | otherwise = getExamplesStr v bs pos

getIntExample:: (Int, String) -> Int
getIntExample (x,_) = x
getStrExample:: (Int, String) -> String
getStrExample (_,y) = y

newValuesInt:: [(Int, String)] -> [Value]
newValuesInt [] = []
newValuesInt (e:es) = newVInt:nextVInt
  where newVInt = ValueInt (getIntExample e, getStrExample e)
        nextVInt = newValuesInt es

getExamplesInt base pos = (sortBy (comparing fst) (generateExamples base pos))
-- FAZER AQUI A ORDENACAO DOS EXEMPLOS ENUMERADOS, NAO PRECISO DELES PARA CALCULAR A ENTROPIA

generateExamples [] _ = []
generateExamples (b:bs) pos = newB:nextB
  where newB = (read (b!!pos) ::Int, last b)
        nextB = generateExamples bs pos

-- selec sort generateAll


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

main = do descriptionInput <- openFile "descricao.txt" ReadMode
          baseInput <- openFile "base.txt" ReadMode
          casesInput <- openFile "caso.txt" ReadMode
          dados <- readAll descriptionInput baseInput casesInput
          

          let description = getDescription dados
          let decison = tail description
          let headerFeatures = init description
          let base = getBase dados
          let cases = getCases dados
          let types = getTypes dados
          
          let features = createFeatures headerFeatures base types
            
          print features

          {-
          putStr "\n\n" 
          print base
          putStr "\n\n"
          print cases
          putStr"\n\n"
          print types
          -}










          hClose descriptionInput
          hClose baseInput
          hClose casesInput
          
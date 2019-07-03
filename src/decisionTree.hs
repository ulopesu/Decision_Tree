{- --PSEUDO-CODIGO

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
import System.IO
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

type Example = (String, String)
newExample:: (String, String) -> Example
newExample (x, y) = (x, y)

type Feature = (String, [Example])
newFeature:: (String, [Example]) -> Feature
newFeature (x, y) = (x, y) 

createFeatures :: [[String]] -> [[String]] -> [Feature]
createFeatures headerFeatures base = createFeaturesAux headerFeatures base 0

createFeaturesAux :: [[String]] -> [[String]] -> Int -> [Feature]
createFeaturesAux [] base pos = []
createFeaturesAux (xs:xss) base pos = (newFeature (head xs, pegaExamples base pos)):(createFeaturesAux xss base (pos+1))

pegaExamples :: [[String]] -> Int -> [Example]
pegaExamples [] pos = []
pegaExamples (xs:xss) pos = (newExample (xs!!pos, last xs)):(pegaExamples xss pos)


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
          
          let features = createFeatures headerFeatures base
            
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
          
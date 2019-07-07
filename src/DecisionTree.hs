module DecisionTree where
import Data.List
import Feature
import Value
import InformationGain


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
      subárvore <- arvoreDecisao(exemplosi, caracteristicas-{melhor}, maioria(exemplos));
      
      
      adicione subárvore como um ramo à árvore com rótulo vi;

  retorne arvore;
-}

data ArvDec = Leaf String | Val Value | Root (String, [ArvDec]) deriving (Show)

--arvoreDecisao (arvDec)
newArvDec:: [Feature] -> ArvDec
newArvDec [] = Leaf []
newArvDec features | sameClass examples = arvStr | otherwise = createRoot feature
  where feature = features!!(bestIGR features)
        examples = gDF feature
        arvStr = Leaf (theClassEx examples)

createRoot:: Feature -> ArvDec
createRoot (Feature (nameF, values, kind)) = Root (nameF, map Val values)

theClassEx :: [String] -> String
theClassEx [] = []
theClassEx (x:xs) = x
                                    
sameClass :: [String] -> Bool
sameClass strings | gLength == 1 = True | otherwise = False
  where groups = group $ sort strings
        gLength = length groups


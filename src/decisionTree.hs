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

data Resposta = Va | NaoVa deriving (Show, Eq)

type Classificao = (String, Resposta)

                    --(Nome, Classificacoes, type)
type Caracterisca =  (String, [Classificaco], String)

data ArvN a = Resposta a | No (Caracterisca) [ArvN a] deriving (Show, Eq) 


main = do descriptionInput <- openFile "descricao.txt" ReadMode
          baseInput <- openFile "base.txt" ReadMode
          casesInput <- openFile "caso.txt" ReadMode
          dados <- readAll descriptionInput baseInput casesInput
          
          print dados
          

          hClose descriptionInput
          hClose baseInput
          hClose casesInput
          
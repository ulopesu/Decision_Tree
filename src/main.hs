import System.IO
import InOut (readAll, getDescription, getBase, getCases, getTypes)
import DecisionTree (newArvDec)
import Feature (createFeatures)
import Results (generateResults)

main = do descriptionInput <- openFile "descricao.txt" ReadMode
          baseInput <- openFile "base.txt" ReadMode
          casesInput <- openFile "caso.txt" ReadMode
          resultsOutput <- openFile "result.txt" WriteMode
          arvOutput <- openFile "arvore.txt" WriteMode

          dados <- readAll descriptionInput baseInput casesInput
          let description = getDescription dados
          let headerFeatures = init description
          let base = getBase dados
          let types = getTypes dados
          let cases = getCases dados

          let features = createFeatures headerFeatures base types
          let arv = newArvDec features 

          --print features
          --putStr "\n\n"
          print arv
          putStr "\n"
          print cases

          hPutStr resultsOutput (generateResults arv cases)

          hClose descriptionInput
          hClose baseInput
          hClose casesInput
          hClose resultsOutput
          hClose arvOutput


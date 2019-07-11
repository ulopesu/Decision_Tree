import System.IO
import Input (readAll, getDescription, getBase, getCases, getTypes)
import DecisionTree 
import Feature (createFeatures, getExFeatures)
import Output 
import Value

main = do descriptionInput <- openFile "descricao.txt" ReadMode
          baseInput <- openFile "base.txt" ReadMode
          casesInput <- openFile "caso.txt" ReadMode
          resultsOutput <- openFile "result.txt" WriteMode
          arvOutput <- openFile "arvore.txt" WriteMode

          dados <- readAll descriptionInput baseInput casesInput
          let description = getDescription dados
          let headerFeatures = init description
          let base = getBase dados
          let cases = getCases dados

          let features = createFeatures headerFeatures base
          let tree = newDecTree features headerFeatures base (mostC $ getExFeatures features)


          print tree
          putStr "\n"

      
          let arv = init (generateArvSTR tree [])
          print arv
          hPutStr arvOutput arv


            {-
          print cases
          let results = (generateResultSTR tree cases)
          print results
          hPutStr resultsOutput results
          -}
          
          hClose descriptionInput
          hClose baseInput
          hClose casesInput
          hClose resultsOutput
          hClose arvOutput


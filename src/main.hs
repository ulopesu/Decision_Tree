import System.IO
import Input (readAll, getDescription, getBase, getCases)
import DecisionTree 
import Feature 
import Output 
import Value
import InformationGain

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

          print features
          print tree
          
          print (iGFeatures features (qtdExpFS features))

          let results = (generateResultSTR tree cases)
          print results
          hPutStr resultsOutput results

          let arv = init (generateArvSTR tree [])
          hPutStr arvOutput arv

          hClose descriptionInput
          hClose baseInput
          hClose casesInput
          hClose resultsOutput
          hClose arvOutput


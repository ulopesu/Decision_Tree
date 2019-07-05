import System.IO
import Input (readAll)
import DecisionTree
import InformationGain

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
          
          let examples = (getExFeatures features)
          let mostCommon = mostC examples





          --let arvDec examples features mostC





          print mostCommon
          --print features
          --putStr "\n"

          --let big = bIG features
          --print big



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
import System.IO
import Input (readAll, getDescription, getBase, getCases, getTypes)
import DecisionTree
import Feature
import Value
import InformationGain

main = do descriptionInput <- openFile "descricao.txt" ReadMode
          baseInput <- openFile "base.txt" ReadMode
          casesInput <- openFile "caso.txt" ReadMode
          dados <- readAll descriptionInput baseInput casesInput

          let description = getDescription dados
          let headerFeatures = init description
          let base = getBase dados
          let cases = getCases dados
          let types = getTypes dados

          let features = createFeatures headerFeatures base types
          let arv = newArvDec features 
          print features
          putStr "\n\n"
          print arv
          putStr "\n"


          hClose descriptionInput
          hClose baseInput
          hClose casesInput
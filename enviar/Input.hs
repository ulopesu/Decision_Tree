module Input where
import System.IO
import Text.Printf

stringToInt :: [String] -> [Int]
stringToInt xs = [read y :: Int | y<-xs]

stringsToInts :: [String] -> [[Int]]
stringsToInts xs = [stringToInt (words y) | y <- xs]

stringsToWords :: [String] -> [[String]]
stringsToWords xs = [words y | y <- xs]

readAll :: Handle -> Handle -> Handle -> IO ([[String]], [[String]], [[String]])
readAll descriptionInput baseInput casesInput  = do description <- readContents descriptionInput
                                                    base <- readContents baseInput
                                                    cases <- readContents casesInput
                                                    return (description, base, cases)
 
removeEmptys [] = []
removeEmptys [[]] = []
removeEmptys (xs:xss) = [xs] ++ (removeEmptys xss)

readContents :: Handle -> IO [[String]]
readContents input = do content <- hGetContents input
                        let linesOfFile = removeEmptys (lines content)
                        let listsOfWords = stringsToWords linesOfFile
                        return listsOfWords


getDescription:: ([[String]], [[String]], [[String]]) -> [[String]]
getDescription (description, _, _) = description

getBase:: ([[String]], [[String]], [[String]]) -> [[String]]
getBase (_, base, _) = base

getCases:: ([[String]], [[String]], [[String]]) -> [[String]]
getCases (_, _, cases) = cases

--let arvMega = criaMegaArv (criaArvs listas)
--putStr (show (criaArvs listas))
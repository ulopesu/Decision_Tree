module Input where
import System.IO

stringToInt :: [String] -> [Int]
stringToInt xs = [read y :: Int | y<-xs]

stringsToInts :: [String] -> [[Int]]
stringsToInts xs = [stringToInt (words y) | y <- xs]

stringsToWords :: [String] -> [[String]]
stringsToWords xs = [words y | y <- xs]

getTypes :: Foldable t => [t a] -> [[Char]]
getTypes [] = []
getTypes (xs:xxs) | length xs > 1 = ["string"] ++ (getTypes xxs)
                  | otherwise = ["int"] ++ (getTypes xxs)

readAll :: Handle -> Handle -> Handle -> IO ([[String]], [[String]], [[String]], [[Char]])
readAll descriptionInput baseInput casesInput  = do description <- readContents descriptionInput
                                                    base <- readContents baseInput
                                                    cases <- readContents casesInput
                                                    let types = getTypes description
                                                    return (description, base, cases, types)
 
readContents :: Handle -> IO [[String]]
readContents input = do content <- hGetContents input
                        let linesOfFile = lines content
                        let listsOfWords = stringsToWords linesOfFile
                        return listsOfWords



--let arvMega = criaMegaArv (criaArvs listas)
--putStr (show (criaArvs listas))
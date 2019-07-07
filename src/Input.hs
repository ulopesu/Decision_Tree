module Input where
import System.IO

stringToInt :: [String] -> [Int]
stringToInt xs = [read y :: Int | y<-xs]

stringsToInts :: [String] -> [[Int]]
stringsToInts xs = [stringToInt (words y) | y <- xs]

stringsToWords :: [String] -> [[String]]
stringsToWords xs = [words y | y <- xs]

findTypes :: Foldable t => [t a] -> [[Char]]
findTypes [x] = []
findTypes (xs:xxs) | length xs > 1 = ["string"] ++ (findTypes xxs)
                   | otherwise = ["int"] ++ (findTypes xxs)



readAll :: Handle -> Handle -> Handle -> IO ([[String]], [[String]], [[String]], [String])
readAll descriptionInput baseInput casesInput  = do description <- readContents descriptionInput
                                                    base <- readContents baseInput
                                                    cases <- readContents casesInput
                                                    let types = findTypes description
                                                    return (description, base, cases, types)
 
readContents :: Handle -> IO [[String]]
readContents input = do content <- hGetContents input
                        let linesOfFile = lines content
                        let listsOfWords = stringsToWords linesOfFile
                        return listsOfWords


getDescription:: ([[String]], [[String]], [[String]], [[Char]]) -> [[String]]
getDescription (description, _, _, _) = description

getBase:: ([[String]], [[String]], [[String]], [[Char]]) -> [[String]]
getBase (_, base, _, _) = base

getCases:: ([[String]], [[String]], [[String]], [[Char]]) -> [[String]]
getCases (_, _, cases, _) = cases

getTypes:: ([[String]], [[String]], [[String]], [String]) -> [String]
getTypes (_, _, _, types) = types

--let arvMega = criaMegaArv (criaArvs listas)
--putStr (show (criaArvs listas))
qsort:: Ord a => [a] -> [a]

qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y <= x] ++ [x] ++ qsort [z | z <-xs, z > x]


pertence x [] = True

annd :: [Bool] -> Bool
annd [x] = x
annd (x:xs) = (annd xs) && x


cct :: [[a]] -> [a]
cct [] = []
cct xss = [xs | xs <- head xss] ++ cct(tail xss)

cct2 :: [[a]] -> [a]
cct2 [] = []
cct2 (x:xs) = x ++ (cct2 xs)

myReplicate :: Int -> a -> [a]
myReplicate 0 a = []
myReplicate n a = [a] ++ (replicate (n-1) a)

mySelect ::  Int -> [a] -> a
mySelect 0 (x:xs) = x
mySelect n (x:xs) = mySelect (n-1) xs

myPertence :: Eq a => a -> [a] -> Bool
myPertence a [] = False
myPertence a (x:xs) = a==x || myPertence a xs

myMerge :: Ord a => [a]->[a]->[a]
myMerge [] ys = ys
myMerge ys [] = ys
myMerge (x:xs) (y:ys) | x > y = [y] ++ (myMerge (x:xs) ys)
 | otherwise = [x] ++ (myMerge xs (y:ys))


--myMergeSort [] = []
--myMergeSort [x] = [x]
--myMergeSort (x:xs) = myMerge () ()

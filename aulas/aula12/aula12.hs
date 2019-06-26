foldRec :: (a -> b -> b) -> b -> [a] -> b

foldRec f v [] = v
foldRec f v (x:xs) = f x (foldRec f v xs)

somafr = foldRec (+) 0

produtfr = foldRec (*) 1

orfr = foldRec (||) False

efr = foldRec (&&) True

tamanhofr = foldRec (\_ n ->  n+1) 0

concatenafr = foldRec (++) []

inverterfr = foldRec (\x xs -> xs ++ [x]) []







get3 = take 3
mult x y z = x*y*z
mult6 = mult 2 3

mult7 :: Int -> [Int]
mult7 n = [7,14..n]

twice f x = f (f x) 

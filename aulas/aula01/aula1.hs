double x = x+x
quadruple x = double (double x)

factorial n = product[1..n]
average ns = sum ns `div` length ns

average2 ns = div(sum ns)(length ns)

n = a `div` length xs
 where
  a=10
  xs=[1,2,3,4,5]

ultimo xs = last xs

ultimo1 xs = xs!!(length xs -1)

ultimo2 xs = head(reverse xs)

inicio xs = take(length xs -1) xs
inicio1 xs = reverse (drop 1 (reverse xs))

add :: (Int, Int) -> Int
add (x,y) = x+y
zeroto :: Int -> [Int]
zeroto n = [0..n]

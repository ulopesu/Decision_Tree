mdc x y = if x == y then x else 
 if x > y then mdc (x-y) y else mdc (y-x) x


--palindromo1 x = reverse (show x) == show x

contaDig x = conta x 0

conta x y = if x > 0 then (conta (x `div` 10) (y+1)) else y

palindromo x = if (contaDig x) <= 1 then True else 
 if (x `mod` 10) /= (x `div` 10 ^ (contaDig x)-1)then False else palindromo((x - 10 ^ (contaDig x)-1) `div` 10)


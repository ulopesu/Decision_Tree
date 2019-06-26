--EXERCICIOS AULA 3
{-TESTE COMENTS-}
maior :: Int -> Int -> Int
maior x y = if x > y then x else y
menor x y = if x < y then True else False

media xs = (sum xs)/ fromIntegral(length xs)

--mediaMaior5 :: [Int] -> Bool
aprovacao1 xs = if media(xs) >= 5 then True else False
aprovacao2 xs = if media(xs) >= 5 then "Aprovados" else "Reprovados"

ordem x y = if (x == y) then "Iguais!" else
 if (x < y) then "Crescente!" else "Decrescente!"

md3 x y z = maior x (maior y z)

tipoTri x y z = if (x == y && x ==z) then "equilatero" else
 if x==y || x==z || z==y then "isosceles" else "escaleno"

ehTri x y z = if (menor x (y+z))  && (menor y (x+z)) && (menor z (x+y)) then True else False

tri x y z = if ehTri x y z then tipoTri x y z else "NAO EH TRIANGULO"

fat x = if x < 2 then 1 else fat (x-1)*x

 


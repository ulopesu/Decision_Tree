indicesLin m n = [(x,y) | x <-[0..m-1], y <-[0..n-1]]

indicesCol m n = [(x,y) | x <-[0..n-1], y <-[0..m-1]]

cont x = [z | y<-x, z<-y]

impares n = [x | x<-[0..2*n], x`mod`2 == 1]

fatores x = [y | y<-[1..x-1], x`mod`y == 0]

primo x = length(fatores x) == 2

primo2 x = fatores x == [0,x]

primos n = [z | z <-[0..n-1], primo z]

ehPerfeito x = x == sum (fatores x)

perfeitos n = [z | z <-[1..n-1], ehPerfeito z || z==1]

fib x y = x:(fib y (x+y))

nfib n = last (take n (fib 0 1))




--Arvore Binária
data Arv a = Nil
           | No (Arv a) a (Arv a)
            deriving Show


--ocorre x (No Nil y Nil) = x == y
ocorre x Nil = False
ocorre x (No ae y ad)   = x == y || ocorre x ae || ocorre x ad


vazia Nil = True
vazia _ = False



ocorre' :: Eq a => a -> Arv a -> Maybe a
ocorre' x Nil = Nothing
ocorre' x (No ae y ad) | x==y = Just x
                       | ocorre' x ae /= Nothing = Just x
                       | otherwise = ocorre' x ad


{-Código do Boina não funciona...

ocorre2 x Nil = Nothing
ocorre2 x (No ae y ad) | x==y = Just x
                       | not $ vazia ae = ocorre2 x ae
                       | otherwise = ocorre2 x ad
-}


{-
(No ( No (No Nil 1 Nil) 2 (No Nil 3 Nil)) 4 (No (No Nil 5 Nil) 6 (No Nil 7 Nil)))

                    4
                /       \
               2         6
             /  \       /  \
            1    3     5    7
           / \  / \   / \   / \
          n  n n   n 4,5|5,5  n  n

-}

-- busca binária em arvore binária já ordenada...
-- No ( No (No Nil 1 Nil) 2 (No Nil 3 Nil)) 4 (No (No Nil 5 Nil) 6 (No Nil 7 Nil))
buscabin :: Ord a => a -> Arv a -> Bool
buscabin _ Nil = False
buscabin x (No ae y ad) | x == y = True
                        | x < y = buscabin x ae
                        | otherwise =  buscabin x ad




{-Fazer inserção e remover...-}

inserebin x Nil = No Nil x Nil
inserebin x (No ae y ad) | y == x = No ae y ad
                         | y < x = No ae y (inserebin x ad)
                         | y > x = No (inserebin x ae) y ad
                                                       

remove _ Nil = Nil
remove x (No ae v ad) | x == v = removeRaiz (No ae v ad) 
                      | x < v = No (remove x ae) v ad
                      | x > v = No ae v (remove x ad)


removeRaiz (No Nil y ad) = ad
removeRaiz (No ae y Nil) = ae
removeRaiz (No ae y ad) = (No ae nv (remove nv ad))
 where nv = maisEsquerda ad
    

maisEsquerda (No Nil v _) = v
maisEsquerda (No ae v ad) = maisEsquerda ae

preOrdem Nil = []
preOrdem (No ae v ad) = [v] ++ (preOrdem ae) ++ (preOrdem ad)

emOrdem Nil = []
emOrdem (No ae v ad) =  (emOrdem ae) ++ [v] ++ (emOrdem ad)

posOrdem Nil = []
posOrdem (No ae v ad) =  (posOrdem ae) ++ (posOrdem ad) ++ [v]


quicksort [] = []
quicksort (x:xs) =  (quicksort small)  ++ [x] ++ (quicksort large)
  where small = [y | y <- xs, y <= x]
        large = [y | y <- xs, y > x]

        
ordB [] = []
ordB [x] = [x]
ordB xxs =  [z] ++ (ordB small) ++ (ordB large)
  where small = [y | y <- xxs, y < z]
        large = [y | y <- xxs, y > z]
        z = xxs!!((length xxs) `div` 2)


criaArv xs = criaArvAux (quicksort xs) Nil
criaArvAux [] arv = arv    
criaArvAux (x:xs) arv = criaArvAux xs (inserebin x arv)

criaArvB xs = criaArvAux (ordB (quicksort xs)) Nil




-- [7,6,5,4,3,2,1]     
-- [1,2,3,4,5,6,7] 

--criaBalanceada arv inser
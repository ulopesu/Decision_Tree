 
--Aula 19


--Redifinir Expressões...

data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr
          deriving Show

instance Eq Expr where
    (Add p1 p2) == (Add p3 p4) = (p1 == p3 && p2 == p4) || (p1 == p4 && p2 == p3)
    (Mul p1 p2) == (Mul p3 p4) = (p1 == p3 && p2 == p4) || (p1 == p4 && p2 == p3)
    (Val p1) == (Val p2) = p1 == p2
    _ == _ = False


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
No ( No (No Nil 1 Nil) 2 (No Nil 3 Nil)) 4 (No (No Nil 5 Nil) 6 (No Nil 7 Nil))

                    4
                /       \
               2         6
             /  \       /  \
            1    3     5    7
           / \  / \   / \   / \
          n  n n   n  n  n  n  n

-}

-- busca binária em arvore binária já ordenada...
-- No ( No (No Nil 1 Nil) 2 (No Nil 3 Nil)) 4 (No (No Nil 5 Nil) 6 (No Nil 7 Nil))
buscabin :: Ord a => a -> Arv a -> Bool
buscabin _ Nil = False
buscabin x (No ae y ad) | x == y = True
                        | x < y = buscabin x ae
                        | otherwise =  buscabin x ad


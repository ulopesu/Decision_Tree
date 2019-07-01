import Data.Maybe

-- 1)
data ArvBin a = Nil 
            | No (ArvBin a) a (ArvBin a) 
             deriving(Show)

-- 2)
insertArv :: (Ord a) => a -> ArvBin a -> ArvBin a
insertArv x Nil = No Nil x Nil
insertArv x (No al y ar) | x > y = (No al y (insertArv x ar))
                         | x < y = (No (insertArv x al) y ar)
                         | otherwise = (No al y ar)

-- 3)
insertListArv :: (Ord a) => [a] -> ArvBin a -> ArvBin a
insertListArv [x] arv = insertArv x arv
insertListArv (x:xs) arv = insertListArv xs (insertArv x arv)

-- 4)
searchListArv :: (Ord a) => ArvBin a -> [a]
searchListArv Nil = []
searchListArv (No al x ar) = (searchListArv al) ++ [x] ++ (searchListArv ar)

-- 5)
instance Functor ArvBin where
 fmap f Nil = Nil
 fmap f (No al x ar) = No (fmap f al) (f x) (fmap f ar)

-- 7)
data Fila a = F [Maybe a] deriving (Show)

-- 8)
vaziaFila = F [Nothing]

-- 9)
insertFila x (F [Nothing]) = F [Just x]
insertFila x (F xs) = F (xs ++ [Just x])

-- 10)
removerFila (F [x]) = F [Nothing]
removerFila (F (x:xs)) = F xs

-- 11)
pegarFila (F (x:xs)) = x

{-
instance Maybe Fila where
 Just (F []) = Nothing
 Just (F )

instance (Eq m) => Eq (Maybe m) where  
 Just x == Just y = x == y  
 Nothing == Nothing = True  
 _ == _ = False  
-}

listX = [5,3,2,4,1,7,8,9,6]
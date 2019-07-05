class RespostaSN a where
 simNao :: a -> Bool

data Xurupita = Xupa | Pita deriving (Eq,Show)

instance RespostaSN Xurupita where
        simNao Xupa = False
        simNao Pita = True 

instance RespostaSN Int where
 simNao 0 = False
 simNao _ = True

instance RespostaSN Integer where
 simNao 0 = False
 simNao _ = True

instance RespostaSN [a] where
 simNao [] = False
 simNao _ = True

instance RespostaSN Bool where
 simNao = id

instance RespostaSN (Maybe a) where
 simNao Nothing = False
 simNao (Just _) = True

simNaoIf :: (RespostaSN y) => y -> String
simNaoIf x | simNao x == True = "Sim\n"
           | simNao x == False = "Nao\n"
           | otherwise = "Nao Sei\n"



class Eq a => Igual a where
 (===) :: a -> a -> Bool
 x === y = x == y


{-
instance Igual Int where
 a === b | a == b = True
         | otherwise = False

instance Igual Integer where
 a === b | a == b = True
         | otherwise = False
-}

data Semaforo = Verde | Vermelho | Amarelo deriving (Show, Eq, Read)




instance Igual Semaforo where

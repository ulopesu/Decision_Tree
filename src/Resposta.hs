module Resposta where

{-
class Resposta a where
 simNao :: a -> RespostaA
   
instance Resposta Int where
 simNao 0 = NaoVa
 simNao _ = Va

instance Resposta Integer where
 simNao 0 = NaoVa
 simNao _ = Va

instance Resposta Bool where
 simNao True = Va
 simNao False = NaoVa

instance Resposta (Maybe a) where
 simNao Nothing = NaoVa
 simNao (Just _) = Va

 {-
instance Resposta String where
 simNao "Va" = True
 simNao _ = False
-}

class Eq a => Igual a where
(===) :: a -> a -> Bool
x === y = x == y

instance Igual Int where
a === b | a == b = True
        | otherwise = False

instance Igual Integer where
a === b | a == b = True
        | otherwise = False

data Semaforo = Verde | Vermelho | Amarelo deriving (Show, Eq, Read)

instance Igual Semaforo where
-}
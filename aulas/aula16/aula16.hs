type Pos = (Int, Int)

origem  :: Pos
origem = (0,0)

esquerda :: Pos -> Pos
esquerda (x,y) = (x-1, y)

type Pos1 = Pos

esquerda1 :: Pos1 -> Pos1
esquerda1 (x,y) = (x-1,y)



type Pair a = (a,a)


dist :: Pos -> Pos -> Float

dist (x1,y1) (x2,y2) = ((fromIntegral (x1-x2))^2 + (fromIntegral (y1-y2)^2))**0.5


type TriploF = (Float,Float,Float)
volume :: TriploF -> Float
volume (a,b,c) = a*b*c

type TriploG a = (a,a,a)
volume1 :: TriploG Float -> Float
volume1 (a,b,c) = a*b*c

data Resposta = Sim | Nao | Desconheco
    deriving (Show, Eq)


chaveia :: Resposta -> Resposta
chaveia Sim = Nao
chaveia Nao = Sim
chaveia Desconheco = Desconheco

 {-
chaveia :: (Eq Resposta) => Resposta -> Resposta
chaveia resp
 | resp == Sim = Nao
 | resp == Nao = Sim
 | otherwise = Desconheco
 -}

respostas :: [Resposta]
respostas = [Sim,Nao,Desconheco]

compara :: Resposta -> Resposta -> Bool
compara x y = x == y


data Figura = Circulo Pos Float | Retangulo Pos Pos 
    deriving (Show)

area :: Figura -> Float
area (Circulo (_,_) r) = pi*r*r
area (Retangulo (x1,y1) (x2,y2)) =  abs (fromIntegral ((x1-x2)*(y1-y2)))

mover :: Figura -> Int -> Int -> Figura
mover (Circulo (c,d) r) a b = (Circulo (c+a,d+b) r)
mover (Retangulo (x1,y1) (x2,y2)) a b = (Retangulo (x1+a,y1+b) (x2+a,y2+b))
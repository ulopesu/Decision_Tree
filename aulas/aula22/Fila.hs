module Fila (
    Fila(..),
    vazia,
    insere,
    remove,
    pega
) where

data Fila a = Fila [Maybe a] deriving Show

vazia :: Fila a
vazia = Fila []

insere :: a -> Fila a -> Fila a
insere e (Fila xs) = Fila (xs ++ [Just e])

remove :: Fila a -> (Maybe a, Fila a)
remove (Fila []) = (Nothing, Fila [])
remove (Fila xs) = (head xs, Fila $ tail xs)

pega :: Fila a -> Maybe a
pega (Fila []) = Nothing
pega (Fila (x:xs)) = x


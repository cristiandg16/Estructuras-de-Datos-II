module P5 where

import Control.Parallel
import Data.List
import Data.Char

data BTree a = Empty | Node Int (BTree a) a (BTree a) deriving Show

-- Int en Node representa la cantidad de nodos totales en el arbol.

(|||) :: a -> b -> (a,b)
x ||| y = (x,y)


-- calcula el n-esimo elemento de una secuencia.
nth :: BTree a -> Int -> a
nth (Node _ leftTree mid rightTree) n | (cantNodos leftTree)  >= (n+1)  = nth leftTree n
									  | (cantNodos leftTree) == n = mid
									  | otherwise = nth r (n - (cantNodos leftTree) - 1)

-- Devuelve la cantidad de nodos de un BTree
cantNodos :: BTree a -> Int 
cantNodos Empty = 0
cantNodos (Node x leftTree mid rightTree) = x

--  inserta un elemento al comienzo de la secuencia.
cons :: a -> BTree a -> BTree a
cons nuevo Empty = Node 1 Empty nuevo Empty
cons nuevo (Node x leftTree mid rightTree) = Node (x+1) (cons nuevo leftTree) mid rightTree

-- la cual dada una función f y un entero n devuelve una secuencia de
-- tamaño n, donde cada elemento de la secuencia es el resultado de aplicar f al ı́ndice del elemento.
tabulate :: (Int -> a) -> Int -> BTree a 
tabulate fun 0 = Empty
tabulate fun n = let m = (div n 2)
					((leftTree,mid),rightTree) = (tabulate fun m) ||| fun (m + 1) ||| (tabulate fun (n-m+1))
				 in Node n leftTree mid rightTree


--  la cual dada una función f y una secuencia s, devuelve el resultado de aplicar f sobre cada elemento de s.
mapBT :: (a -> b) -> BTree a -> BTree a
mapBT funcion Empty = Empty
mapBT funcion (Node n leftTree mid rightTree) = let ((left,m),right) = mapBT funcion left ||| funcion m ||| mapBT funcion right
												in Node n left m right


-- tal que dados un entero n y una secuencia s devuelve los primeros n elementos de s.
takeBT :: Int -> BTree a -> BTree a
takeBT _ Empty = Empty
takeBT 0 _ = Empty
takeBT n (Node x leftTree mid rightTree) | n == (cantNodos leftTree) = leftTree
										 | n < (cantNodos leftTree) = takeBT n leftTree
										 | n == ((cantNodos leftTree)+1) = Node ((cantNodos leftTree)+1) leftTree mid Empty
										 | otherwise = Node n leftTree mid (takeBT (n - (cantNodos leftTree) -1) rightTree)


--  tal que dados un entero n y una secuencia s devuelve la secuencia s sin los primeros n elementos.
dropBT :: Int -> BTree a -> BTree a
dropBT n Empty = Empty
dropBT 0 (Node x leftTree mid rightTree) = (Node x leftTree mid rightTree)
dropBT n (Node x leftTree mid rightTree) | n == (cantNodos leftTree) = Node (x-n) Empty mid rightTree
										 | n < (cantNodos leftTree) = Node (x-n) (dropBT n leftTree) mid rightTree
										 | otherwise = dropBT (n - (cantNodos l) - 1) rightTree
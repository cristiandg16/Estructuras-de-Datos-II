module Lab02 where

import Data.List
import Data.Char

{-
1) Inferir, de ser posible, los tipos de las siguientes funciones:
(puede suponer que sqrt :: Float → Float)
-}

-- a)
modulus :: [Float] -> Float
modulus = sqrt . sum . map (^2)

-- b)
vmod :: [[Float]] -> [Float]
vmod [] = []
vmod (v : vs) = modulus v : vmod vs

--------------------------------------------------------------------------

-- 2) Dado el siguiente tipo para representar números binarios:
type NumBin = [Bool]

{- donde el valor False representa el número 0 y True el 1. Definir las siguientes operaciones tomando como convención
 una representación Little-Endian (i.e. el primer elemento de las lista de dı́gitos es el dı́gito menos significativo del
 número representado).-}

--a) suma binaria
sumaBin :: NumBin -> NumBin -> NumBin
sumaBin xs ys | length xs == length ys = sumaE xs ys False
              | length xs < length ys = sumaE (reverse (rellenaZeros (reverse xs) ((length ys) - (length xs)))) ys False
              | otherwise = sumaE xs (reverse (rellenaZeros (reverse ys) ((length xs) - (length ys)))) False

sumaE :: NumBin -> NumBin -> Bool -> NumBin
sumaE [] [] acarreo = []
sumaE (x:xs) (y:ys) acarreo  | x == False && y == False && acarreo == False = False :(sumaE xs ys False)
    | x == False && y == True && acarreo  == False = True :(sumaE xs ys False)
    | x == False && y == False && acarreo == True  = True :(sumaE xs ys False)
    | x == False && y == True && acarreo  == True  = False :(sumaE xs ys True)
    | x == True && y  == False && acarreo == False = True :(sumaE xs ys False)
    | x == True && y  == False && acarreo == True  = False :(sumaE xs ys True)
    | x == True && y  == True && acarreo  == False = False :(sumaE xs ys True)
    | x == True && y  == True && acarreo  == True  = True :(sumaE xs ys True)

rellenaZeros :: [Bool] -> Int -> [Bool]
rellenaZeros xs index = if index == 0 then xs else False : rellenaZeros xs (index - 1)

--b) producto binario
prodBin :: NumBin -> NumBin -> NumBin
prodBin [] ys = []
prodBin (False:xs) ys =  prodBin xs (False:ys)
prodBin (x:xs) ys = sumaBin (prodBin xs (False:ys)) ys

prodAux :: Bool -> [Bool] -> [Bool]
prodAux bit [] = []
prodAux bit (x:xs) | bit == True && x == True = True : (prodAux bit xs)
    | bit == True && x == False = False : (prodAux bit xs)
    | bit == False = False : (prodAux bit xs)

-- c) cociente y resto de la división por dos

cociente :: NumBin -> NumBin
cociente x = (reverse (init (reverse x)) ++ [False])

modBin :: NumBin -> NumBin
modBin xs = head xs : rellenaZeros [] (length (xs) - 1) 

-- 3) Dado el tipo de datos \\ 0 1 1 0 -> 0 1 1 -> 1 1 0
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a

{-a) Implementar las operaciones de este tipo algebraico teniendo en cuenta que:

* Las funciones de acceso son headCL, tailCL, isEmptyCL, isCUnit.
* headCL y tailCL no están definidos para una lista vacı́a.
* headCL toma una CList y devuelve el primer elemento de la misma (el de más a la izquierda).
* tailCL toma una CList y devuelve la misma sin el primer elemento.
* isEmptyCL aplicado a una CList devuelve True si la CList es vacı́a (EmptyCL) o False en caso contrario.
* isCUnit aplicado a una CList devuelve True sii la CList tiene un solo elemento (CUnit a) o False en caso contrario.-}

deCListToList :: CList a -> [a]
deCListToList EmptyCL = []
deCListToList (CUnit a) = [a]
deCListToList (Consnoc x xs y) = [x] ++ deCListToList xs ++ [y]

deListToCList :: [a] -> CList a
deListToCList [] = EmptyCL
deListToCList [x] = CUnit x
deListToCList (x:xs) = Consnoc x (deListToCList (init xs)) (last xs)

headCL :: CList a -> a
headCL (CUnit x) = x
headCL (Consnoc x _ _) = x

tailCL :: CList a -> CList a
tailCL (CUnit x) = EmptyCL
tailCL (Consnoc x xs y) = case xs of
    EmptyCL -> CUnit y
    CUnit xs' -> Consnoc xs' EmptyCL y
    otherwise -> Consnoc (headCL xs) (tailCL xs) y

isEmptyCL :: CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL _ = False

isCUnit :: CList a -> Bool
isCUnit (CUnit _) = True
isCUnit _ = False


-- b) Definir una función reverseCL que toma una CList y devuelve su inversa.

reverseCL :: CList a -> CList a
reverseCL EmptyCL = EmptyCL
reverseCL (CUnit x) = CUnit x
reverseCL (Consnoc x xs y) = case xs of
    EmptyCL -> Consnoc y EmptyCL x
    CUnit xs' -> Consnoc y (CUnit xs') x
    otherwise -> Consnoc y (reverseCL xs) x

-- c) Definir una función inits que toma una CList y devuelve una CList con todos los posibles inicios de la CList.

initCL :: CList a -> CList a
initCL EmptyCL = EmptyCL
initCL (CUnit x) = EmptyCL
initCL (Consnoc x xs y) = case xs of
    EmptyCL -> CUnit x
    otherwise -> Consnoc x (initCL xs) (headCL (reverseCL xs))

initsCL :: CList a -> CList(CList a)
initsCL EmptyCL = EmptyCL
initsCL (CUnit x) = Consnoc EmptyCL EmptyCL (CUnit x)
initsCL (Consnoc x xs y) = Consnoc (CUnit x) (Consnoc EmptyCL (initsCL xs) EmptyCL) EmptyCL

-- d) Definir una función lasts que toma una CList y devuelve una CList con todas las posibles terminaciones de la CList.

lastsCL :: CList a -> CList (CList a)
lastsCL EmptyCL = EmptyCL
lastsCL (CUnit x) = Consnoc (CUnit x) EmptyCL EmptyCL
lastsCL (Consnoc x xs y) = Consnoc EmptyCL (Consnoc EmptyCL (lastsCL xs) EmptyCL ) (CUnit y)

-- e) Definir una función concatCL que toma una CList de CList y devuelve la CList con todas ellas concatenadas

($++$) :: CList a -> CList a -> CList a
($++$) EmptyCL y = y
($++$) x EmptyCL = x
($++$) (CUnit x) y = Consnoc x (initCL y) (headCL (reverseCL y))
($++$) x y = Consnoc (headCL x) (tailCL x $++$ initCL y) (headCL (reverseCL y))

concatCL :: CList (CList a) -> CList a
concatCL EmptyCL = EmptyCL
concatCL (CUnit x) = x
concatCL (Consnoc x xs y) = x $++$ (concatCL xs) $++$ y

-- 4) Dada las siguientes representaciones de árboles generales y de árboles binarios

data GenTree a = EmptyG | NodeG a [GenTree a]

data BinTree a = EmptyB | NodeB (BinTree a) a (BinTree a)

{-defina una función g2bt que dado un árbol nos devuelva un árbol binario de la siguiente manera:
la función g2bt reemplaza cada nodo n del árbol general (NodeG) por un nodo n' del árbol binario (NodeB ), donde
el hijo izquierdo de n' representa el hijo más izquierdo de n, y el hijo derecho de n' representa al hermano derecho
de n, si existiese (observar que de esta forma, el hijo derecho de la raı́z es siempre vacı́o).-}

g2bt :: GenTree a -> BinTree a
g2bt EmptyG = EmptyB
g2bt (NodeG n ns) = NodeB (gtreeToBtree ns) n EmptyB where
    gtreeToBtree [] = EmptyB
    gtreeToBtree ((NodeG n ns) : ms) = NodeB (gtreeToBtree ns) n (gtreeToBtree ms) 

-- 5) Definir las siguientes funciones sobre árboles binarios de búsqueda (bst):

data BST a = Hoja | Nodo (BST a) a (BST a)
--a) maximum, que calcula el máximo valor en un bst.

maximumBST :: Ord a => BST a -> a
maximumBST (Nodo l a Hoja) = a
maximumBST (Nodo l a r) = maximumBST r

--b) checkBST, que chequea si un árbol binario es un bst.

checkBST :: Ord a => BinTree a -> Bool
checkBST EmptyB = True
checkBST (NodeB EmptyB x EmptyB) = True
checkBST (NodeB EmptyB x r) = ( x < (datoB r)) && checkBST r
checkBST (NodeB l x EmptyB) = ( x >= (datoB l)) && checkBST l
checkBST (NodeB l x r) = ((datoB l <= x) && (datoB r >  x)) && (checkBST l && checkBST r) 

datoB :: BinTree a -> a    
datoB (NodeB _ x _) = x

----
{-
checkBST1 :: Ord a => BinTree a -> Bool
checkBST1 EmptyB = True
checkBST1 (NodeB l a r) | ((l == EmptyB) && (r == EmptyB) == True) = True
                        | ((l == EmptyB) && (datoNodeB r > a) && checkBST1 r) == True = True 
                        | ((r == EmptyB) && (datoNodeB l <= a) && checkBST1 l) == True = True
                        | ((datoNodeB l <= a) && (datoNodeB r > a) && (checkBST1 r) && (checkBST1 l)) == True = True where
                            datoNodeB (NodeB _ x _) = x
-}
----
checkBSTwithList :: Ord a => BinTree a -> Bool
checkBSTwithList EmptyB = True
checkBstwithList (NodeB l a r) = listMinToMax (inorder (NodeB l a r))

inorder :: BinTree a -> [a]
inorder EmptyB = []
inorder (NodeB l a r) = inorder l ++ [a] ++ inorder r

listMinToMax :: Ord a => [a] -> Bool
listMinToMax [] = True
listMinToMax [_] = True
listMinToMax (x:y:xs) = (x <= y) && listMinToMax (y:xs)
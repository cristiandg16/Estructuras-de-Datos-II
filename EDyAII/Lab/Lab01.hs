module Lab01 where

import Data.List
import Data.Char

{- 
1) Corregir los siguientes programas de manera que sean aceptados por GHCi
-}

--a)
not b = case b of           -- not' Funciona tambien.
        True -> False   
        False -> True

--b)
in' [x] = []
in' (x:xs) = x : in' xs
in' [] = error "empty list"

--c)
length' [] = 0
length' (_:l) = 1 + length' l

--d)
list123 = 1:2:3:[]

--e)
[] ++! ys = ys
(x:xs) ++! ys = x : (xs ++! ys)

--f)
addToTail x xs = map (+x) (tail xs)

--g)
listmin xs = head(sort xs)

--h)
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap f (smap f xs) -- f x : (smap f xs)

--------------------------------------------------------------------------

{-2
Definir las siguientes funciones y determinar su tipo:
-}

--a) five, que dado cualquier valor devuelve 5

five :: a -> Int
five x = 5

--b) apply, que toma una funcion y un valor, y devuelve el resultado de aplicar la funcion al valor dado

apply :: (a -> b) -> a -> b
apply f x = f x
        
--c) ident, la funcion identidad

ident :: a -> a
ident x = x

--d) first, que toma un par ordenado y devuelve su primera componente.

first :: (a,b) -> a
first (a,b) = a

--e) derive, que aproxima la derivada de una funcion dada en un punto dado.

derive :: (Float -> Float) -> Float -> Float -> Float
derive f a h = (f(a + h) - f (a)) / h

--f) sign, la funcion signo

sign :: Int -> Int
sign x  | x < 0 = -1
        | x == 0 = 0
        | otherwise = 1

--g) vabs, la funcion valor absoluto con sign y sin sign

vabsss :: Int -> Int
vabsss x = if x >= 0 then x else -x

vabscs :: Int -> Int
vabscs x | sign x >= 0 = x
         | otherwise = -x

--h) pot, que toma un entero y un numero, y devuelve el resultado de elevar el segundo a la
--   potencia dada por el primero.

pot :: Int -> Float -> Float
pot y x | y == 0 = 1
pot y x | y > 0 = x * pot (y-1) x
pot y x | y < 0 = 1/(pot (-1*y) x)

--i) xor, el operador de disyuncion exclusiva

xor :: Bool -> Bool -> Bool
xor a b | a == b = False
        | otherwise = True

--j) max3, que toma tres numeros enteros y devuelve el maximo entre ellos.

max3 :: Int -> Int -> Int -> Int
max3 a b c | a>=b && a>=c = a
           | b>=a && b>=c = b
           | c>=a && c>=b = c

--k) swap, que toma un par y devuelve el par con sus componentes invertidas.
swap :: (Int,Int) -> (Int,Int)
swap (x,y) = (y,x)

--------------------------------------------------------------------------

{-
3) Definir una función que determine si un año es bisiesto o no, de acuerdo a la 
   siguiente definicion:
   año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero.
   Se repite cada cuatro años, a excepción del último de cada siglo cuyo número de centenas
   no sea múltiplo de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)
¿Cuál es el tipo de la función definida?
-}

bisiesto :: Int -> Bool
bisiesto x | mod x 100 == 0 && mod x 400 == 0 = True
           | mod x 4 == 0   && mod x 100 /= 0 = True
           | otherwise = False

--------------------------------------------------------------------------

{-
4) Defina un operador infijo *$ que implemente la multiplicación de un
vector por un escalar. Representaremos a los vectores mediante listas
de Haskell. Así, dada una lista ns y un número n, el valor ns *$ n
debe ser igual a la lista ns con todos sus elementos multiplicados por n. Por ejemplo,

[ 2, 3 ] *$ 5 == [ 10 , 15 ].

El operador *$ debe definirse de manera que la siguiente expresión sea válida:

v = [1, 2, 3] *$ 2 *$ 4
-}

(*$) :: [Int] -> Int -> [Int]
ns *$ n = map f1 ns
        where f1 x = x*n

--------------------------------------------------------------------------

{-
Definir las siguientes funciones usando listas por comprensión:
-}

--a) 'divisors', que dado un entero positivo 'x' devuelve la
--  lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)

divisors :: Int -> [Int]
divisors x | x <= 0 = []
           | otherwise = [n | n <- [1..x] , mod x n == 0 ]

--b) 'matches', que dados un entero 'x' y una lista de enteros descarta
--  de la lista los elementos distintos a 'x'

matches :: Int -> [Int] -> [Int]
matches x ns = [n | n <- ns , n == x] 

--c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
--  '(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
--  donde 0 <= a, b, c, d <= 'n'

cuadrupla :: Int -> [(Int,Int,Int,Int)]
cuadrupla n = [(a,b,c,d) | a <- [0..n],b <- [0..n],c <- [0..n],d <- [0..n], a^2 + b^2 == c^2 + d^2]

-- cuadrupla2 :: Int -> [(Int,Int,Int,Int)]
-- cuadrupla2 n = [(a,b,c,d) | a,b,c,d <- [0..n], a^2 + b^2 == c^2 + d^2] Forma mas linda?

--(d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
--  'xs' sin elementos repetidos

unique :: [Int] -> [Int]
unique xs = [x | (x,i) <- zip xs [0..], Prelude.not (elem x (take i xs))]

--------------------------------------------------------------------------

{- 
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'. 

[1,2,3] * [2,3,4] = 20
-}

-- productoDupla :: (Int,Int) -> Int
-- productoDupla (x,y) = x*y

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [productoDupla(x,y) | (x,y) <- zip xs ys]
        where productoDupla(x,y) = x*y

--------------------------------------------------------------------------

{-

7) Definir mediante recursión explícita las siguientes funciones y escribir su tipo más general:

-}

--a) 'suma', que suma todos los elementos de una lista de números

suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs  

--b) 'alguno', que devuelve True si algún elemento de una
--  lista de valores booleanos es True, y False en caso contrario

alguno :: [Bool] -> Bool
alguno [] = False
alguno (x:xs) = if x == True then True else alguno xs

--c) 'todos', que devuelve True si todos los elementos de
--  una lista de valores booleanos son True, y False en caso contrario

todos :: [Bool] -> Bool
todos [] = False
todos [True] = True
todos (x:xs) = if x == False then False else todos xs

--d) 'codes', que dada una lista de caracteres, devuelve la lista de sus ordinales

codes :: [Char] -> [Int]
codes [] = []
codes (x:xs) = (ord x) : codes xs

--e) 'restos', que calcula la lista de los restos de la
--  división de los elementos de una lista de números dada por otro número dado

restos :: [Int] -> Int -> [Int]
restos [] n = []
restos xs 0 = xs
restos (x:xs) n = mod x n : restos xs n

--f) 'cuadrados', que dada una lista de números, devuelva la lista de sus cuadrados

cuadrados :: [Float] -> [Float]
cuadrados [] = []
cuadrados (x:xs) = pot 2 x : cuadrados xs

--g) 'longitudes', que dada una lista de listas, devuelve la lista de sus longitudes

longitudes :: [[a]] -> [Int]
longitudes [] = []
longitudes (xs:xss) = length xs : longitudes xss 

--h) 'orden', que dada una lista de pares de números, devuelve
--  la lista de aquellos pares en los que la primera componente es
--  menor que el triple de la segunda

orden :: [(Int,Int)] -> [(Int,Int)]
orden [] = []
orden ((x,y):xs) = if x < (3*y) then (x,y) : orden xs else orden xs
        
--i) 'pares', que dada una lista de enteros, devuelve la lista
--  de los elementos pares

pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) = if mod x 2 == 0 then x : pares xs else pares xs

--j) 'letras', que dada una lista de caracteres, devuelve la
--  lista de aquellos que son letras (minúsculas o mayúsculas)

letras :: [Char] -> [Char]
letras [] = []
letras (x:xs) = if isDigit x == False then x : letras xs else letras xs

--k) 'masDe', que dada una lista de listas 'xss' y un
--  número 'n', devuelve la lista de aquellas listas de 'xss' con longitud mayor que 'n' 

masDe :: [[a]] -> Int -> [[a]]
masDe [] n = []
masDe (xs:xss) n = if length xs > n then xs : masDe xss n else masDe xss n

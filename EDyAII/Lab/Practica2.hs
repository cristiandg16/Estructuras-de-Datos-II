module PruebaP2 where

import Data.List
import Data.Char



{-
    "Test", donde Test F x = F x == x+2
-}

f1 :: Int -> Int
f1 x = x + 2

test :: (Int -> Int) -> Int -> Int
test f1 x = f1 x
    
{-
    "esMenor, donde esMenor y z = y < z"
-}

esMenor :: Int -> Int -> Bool
esMenor y z = y < z

{-
    "eq", donde eq a b = a == b
-}

eq :: Int -> Int -> Bool
eq a b = if a==b then True else False

{-
    "showVal" donde showVal x = "valor:" ++ show x
-}

showVal :: (Show a) => a -> [Char]
showVal x = "Valor:" ++ show x

--------------------------------------------------------------------------
-- 3)

fa1 :: (Int -> Int) -> Int
fa1 f2 = f2 1
    
fb1 :: Int -> (Int -> Int)
fb1 x = (+x)

fd1 :: Int -> Bool
fd1 x = x==x


--------------------------------------------------------------------------

--4)
ej4a = if true then false else true
    where false = True;true = False

-- ej4b = if if then then else else

ej4c = False == (5 >= 4)

ej4e = 1 + if ('a'<'z') then -1 else 0

ej4f = if fst p then fst p else snd p 
    where p = (True,False)

-----------------------------------------------------------------------------
--5)

--a) f5 x = let (y,z) = (x,x) in y
f5 x = x

--b) greater (x,y) = if x > y then True else False
greater (x,y) = True == (x>y)

--c) f (x,y) = let z = x + y in g(z,y) where g (a,b) = a - b
f6 (x,y) = x

-----------------------------------------------------------------------------
--6)
smallest :: (Int,Int,Int) -> Int

smallest (x,y,z) | x <= y && x <= z = x
                 | y <= x && y <= z = y
                 | z <= x && z <= y = z

smallest1 :: Ord a => a -> a -> a -> a
smallest1 = \x y z -> if x <= y && x <= z then x 
                      else if y <= x && y <= z then y
                      else z 

---------------
--b)
--second :: a -> a
--second x = \x -> x

second1 = \_ -> (\x -> x)

---------------
--c)
-- twice f x = f (f x)

twice :: (Int -> Int) -> Int -> Int
twice = \f x -> f (f x)

----------------
--d)
-- flip f x y = f y x

flip1 :: (Int -> Int -> Int) -> Int -> Int -> Int
flip1 = \f x y -> f y x

----------------
--e)
-- inc = (+1)

inc :: Int -> Int
inc = \x -> x + 1

--------------------------------------------------------------------------

--a) iff = \x -> \y -> if x then not y else y

iff :: Bool -> Bool -> Bool
iff x y = if x then not y else y

--b) alpha = \x -> x

alpha :: a -> a
alpha x = x

--------------------------------------------------------------------------

--9)

zip3R :: [a] -> [a] -> [a] -> [(a,a,a)]

zip3R [] (y:ys) (z:zs) = []
zip3R (x:xs) [] (z:zs) = [] 
zip3R (x:xs) (y:ys) [] = []
zip3R (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3R xs ys zs

zip3zip :: [a] -> [a] -> [a] -> [(a,a,a)]

zip3zip xs ys zs = funcZip $ zip (zip xs ys) zs where
    funcZip [] = []
    funcZip (((x,y),z) : xs) = (x,y,z) : (funcZip xs)

--------------------------------------------------------------------------
--11)

modulus :: [Float] -> Float
modulus xs = sqrt (sum (map f11 xs)) where
    f11 x = x^2

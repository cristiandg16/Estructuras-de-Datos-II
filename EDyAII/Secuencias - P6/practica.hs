

import Par
import Seq
import ListSeq



mayores :: Seq s => s Int  -> Int
mayores xs = let (a,b) = scanS max 0 xs
                 xs1 = appendS (dropS a 1) (singletonS b)
                 fun index = if (nthS xs1 index) < (nthS xs1 (index + 1)) then 1
                             else 0
             in reduceS (+) 0 ((tabulateS fun ((lengthS xs1)-1)  :: [Int]))
             
xs = fromList [1,2,5,3,5,7,9] :: [Int]

-----------------------------

--fibSeq ::  Seq s => Int -> s Int
fibSeq n = let seqMatrix = (tabulateS fib n :: [(Int,Int,Int,Int)])
               fib idx = (1,1,1,0) 
               seqM = (scanS prodMatrix (1,0,0,1) seqMatrix) 
               prodMatrix (a,b,c,d) (e,f,g,h) = (a*e+c*f,a*g+c*h,b*e+d*f,b*g+d*h)
               deMaE (a,b,c,d) = b 
            in mapS deMaE (fst seqM) 

------------------------------

--aguaHist :: Seq s => s Int -> Int

aguaHist xs = let (a,b) = scanS max 0 xs 
                  (a1,b1) = scanS max 0 (reverseS xs)
                  maxL = appendS (dropS a 1) (singletonS b)
                  maxR = reverseS (appendS (dropS a1 1) (singletonS b1))
                  skere = tabulateS (\index -> max 0 ((min (nthS maxL index) (nthS maxR index)) - nthS xs index)) (lengthS xs) :: [Int]
              in reduceS (+) 0 skere

xs1 = [2,3,4,7,5,2,3,2,6,4,3,5,2,1]
--maxR = [7,7,7,7,6,6,6,6,6,5,5,5,2,1]
(a,b) = scanS max 0 xs1
maxL = appendS (dropS a 1) (singletonS b)
(a1,b1) = scanS max 0 (reverseS xs1)
maxR = reverseS (appendS (dropS a1 1) (singletonS b1))

reverseS xs = tabulateS (\indice -> nthS xs (lengthS xs - indice - 1)) (lengthS xs) :: [Int]


----------------------------------------------

data Paren = Open | Close

matchP :: Seq s => s Paren -> (Int,Int)
matchP xs = (reduceS f (0,0) (mapS match xs)) where
    match Open = (0,1)
    match Close = (1,0)
    f (closeL,openL) (closeR,openR) = closeL + max 0 (closeR - openL) ||| max 0 (openL - closeR) + openL 

matchParen :: Seq s => s Paren -> Bool
matchParen xs = (matchP xs) == (0,0)

---------

xsParen = [Open,Close,Close,Open,Close,Open,Close] 

{-
matchParenV2 :: Seq s => s Paren -> Bool
matchParenV2 xs = let (sec,res) = scanS (+) 0 (mapS matchSum xs)
                      matchSum Open  = 1
                      matchSum Close = -1
                      filteR = filterS check sec
                      check x = if x >= 0 then False
                                else True
                    in (filteR == (emptyS :: s Int)) && (res == 0)

-}

mV2 :: Seq s => s Paren -> Bool
mV2 xs = let (sec,res) = scanS (+) 0 (mapS matchSum xs)
             matchSum Open  = 1
             matchSum Close = -1
         in (0 == reduceS min 0 sec) && (res == 0)


-------------------------------------------------------------------

--cantMultiplos :: Seq s => s Int -> Int
--cantMultiplos xs = reduceS (mod (nthS xs (lengthS xs -1)) ) 1 xs

--x = reduceS g 0 xsMult where
--    g 0 j = if mod (nthS xsMult 0) j == 0 then 1
--            else 0

cantMultiplos xs = reduceS (+) 0 (tabulateS f (lengthS xsMult) :: [Int]) where
                f i = reduceS (+) 0 (mapS (modulaR i) (noRepeat i))
                noRepeat i = dropS xsMult (i+1)
                modulaR i x = if (mod (nthS xsMult i) x) == 0 then 1 
                              else 0


xsMult = [] 



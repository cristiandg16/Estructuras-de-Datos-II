module ArrSeq where

import Seq
import Par 
import qualified Arr as A
-- emptyS :: A.Arr Int

instance Seq A.Arr where
	emptyS = A.empty

	singletonS x = A.fromList [x]

	lengthS = A.length

	joinS = A.flatten

	tabulateS = A.tabulate

	mapS f xs = tabulateS (f . (xs A.!) ) len
		where len = lengthS xs

	filterS f xs = joinS (mapS fun xs) where
		fun x | f x = singletonS x
			  | otherwise = emptyS

	appendS xs ys = A.flatten (A.fromList [xs, ys])

	takeS xs n = A.subArray 0 n xs

	dropS xs n = A.subArray n ((lengthS xs)-n) xs

	showtS xs | lengthS xs == 0 = EMPTY
			  | lengthS xs == 1 = ELT (xs A.! 0)
			  | otherwise  = NODE (takeS xs m) (dropS xs m)
			  	where m = div (lengthS xs) 2

	showlS xs | lengthS xs == 0 = NIL
			  | otherwise = CONS (xs A.! 0) (dropS xs 1)


	reduceS f e xs | lengthS xs == 0 = e
				   | lengthS xs == 1 = f e (xs A.! 0)
				   | otherwise = reduceS f e (contract f xs) -- reduceS f e (Techo n/2)
				  
				   		 

	scanS f e xs | lengthS xs == 0 = (emptyS,e)
				 | lengthS xs == 1 = (singletonS e, f e (xs A.! 0))
				 | otherwise = expand f xs (scanS f e (contract f xs))


contract f xs | lengthS xs <= 1 = xs 
			  | otherwise = let (y,ys) = (f (xs A.! 0) (xs A.! 1)) ||| contract f (dropS xs 2)
							in cons y ys
									where cons x xs = tabulateS f ((lengthS xs) + 1) where
										f 0 = x
										f n = (xs A.! (n-1))

	

	
expand f ys (xs,x) = (tabulateS g (lengthS ys), x)
	where g index | even index = y
			  	  | otherwise = f y (ys A.! (index-1))
				  	where y = xs A.! (div index 2)


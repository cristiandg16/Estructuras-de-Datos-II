module ListSeq where

-- Representación de secuencias como Listas

import Seq
import Par

contractS :: (a->a->a) -> [a] -> [a]
contractS f [ ] = [ ]
contractS f [x] = [x]
contractS f (x1:x2:xs) = let (y,ys) = f x1 x2 ||| contractS f xs
						in y:ys


instance Seq [] where
	--Representa la secuencia vacía con la lista vacía.
	emptyS = [] 

	--Crea una secuencia unitaria, es decir, con un solo elemento.
	singletonS a = [a]	

	--Retorna la longitud de una secuencia.
	lengthS [] = 0		
	lengthS (x:xs) = 1 + lengthS(xs)

	-- Devuelve el elemento n-esimo, pasado como argumento de la secuencia correspondiente.
	nthS (x:xs) 0 = x		
	nthS (x:xs) n = nthS xs (n-1) 

	-- Crea una secuencia a partir de aplicarle f a una secuencia de longitud n.
	tabulateS f 0 = []
	tabulateS f n = let (x,xs) = f(0) ||| tabulateS (f . (+1)) (n-1)
					in x:xs

	-- Aplica f a cada elemento de la secuencia.
	mapS f [] = []
	mapS f (x:xs) = let (y,ys) = f(x) ||| mapS f xs
					in y:ys

	-- Devuelve una secuencia con los elementos que cumplen con la condicion pasada como argumento.
	filterS _ [] = []
	filterS cond (x:xs) | es_valido = x:ys
					 	| otherwise = ys
					  where (es_valido,ys) = cond(x) ||| filterS cond xs

	-- Concatena dos secuencias.
	appendS = (++)

	-- Devuelve una secuencia con los n primeros elementos.
	takeS xs n = take n xs

	-- Devuelve una secuencia con los n ultimos elementos.
	dropS xs n = drop n xs

	showtS [] = EMPTY
	showtS [x] = ELT x
	showtS xs  = 
		let tamanio = quot (lengthS xs) 2
		    (left,right) = takeS xs tamanio ||| dropS xs tamanio
		 in NODE left right

	showlS [] = NIL
	showlS (x:xs) = CONS x xs 

	joinS = concat

	reduceS f b [] = b
	reduceS f b [x] = f b x
	reduceS f b xs  = reduceS f b (contractS f xs)

	expandS _ [] _ = []
	expandS _ [] [] = []
	expandS _ [y] [] = [y]
	expandS f (y : ys) (x : _ : xs) = y : f y x : expandS f ys xs
	
	scanS b [] = ([], b)
	scanS f b [x] = ([b], f b x)
	scanS f b xs = (expand f ys xs, y)
		where (ys, y) = scanS f b (contractS f xs)

	fromList = id


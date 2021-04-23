--Genga Cristian

--4)

	tad PriorityQueue (a:Set) where
		import Ord,Bool

		vaciaPQ :PriorityQueue a
		ponerPQ :a -> Int -> PriorityQueue a -> PriorityQueue a
		sacarPQ :PriorityQueue a -> PriorityQueue a
		primeroPQ :PriorityQueue a -> a
		esVaciaPQ :PriorityQueue a -> Bool
		unionPQ :PriorityQueue a -> PriorityQueue a -> PriorityQueue a

		ponerPQ x prioridadx (ponerPQ y prioridady cola) | (prioridady == prioridadx) == True = ponerPQ y prioridady cola
														 | otherwise = ponerPQ x prioridadx (ponerPQ y prioridady cola)

		sacarPQ (ponerPQ x prioridadx vaciaPQ) = vaciaPQ
		sacarPQ (ponerPQ x prioridadx (ponerPQ y prioridady cola)) = if prioridady > prioridadx then ponerPQ x prioridadx (sacarPQ y prioridady cola)
																	 else ponerPQ y prioridady (sacarPQ x prioridadx cola)

		primeroPQ (ponerPQ x prioridadx vaciaPQ) = x
		primeroPQ ponerPQ x prioridadx (ponerPQ y prioridady cola) = if prioridadx > prioridady then x
																	 else y

		esVaciaPQ vaciaPQ = True
		esVaciaPQ (ponerPQ _ _) = False

		unionPQ c1 vaciaPQ = c1
		unionPQ c1 (ponerPQ x prioridadx c2) = ponerPQ x prioridadx (unionPQ c1 c2)  

	{-
	
	(E) = Pertenece
	(/E) = No Pertenece

	vaciaPQ = {}

	
	ponerPQ x prioridadx c = {(x,prioridadx)} U {(y,prioridady) (E) c / prioridadx != prioridady}
	
	sacarPQ (ponerPQ x prioridad vaciaPQ) = sacarPQ {(x,prioridadx)} = {}
	sacarPQ (ponerPQ x prioridadx (ponerPQ y prioridady c)) = sacarPQ {(x1,prioridadx1), (x2,prioridadx2), (x3,prioridadx3), ... } =
															= {(xi,prioridadxi) (E) c / prioridadxi < max({prioridadx1, prioridadx2, ...})} 

	primeroPQ (ponerPQ x prioridadx vaciaPQ) = primeroPQ {(x,prioridadx)} = x
	primeroPQ ponerPQ x prioridadx (ponerPQ y prioridady cola) = primeroPQ {(x1,prioridadx1), (x2,prioridadx2), (x3,prioridadx3), ...} =
															   = (xi,prioridadxi) / prioridadxi = max({prioridadx1, prioridadx2, ...})

	union c1 vaciaPQ = union {(x1,prioridadx1), (x2,prioridadx2), (x3,prioridadx3), ... } {} = {(x1,prioridadx1), (x2,prioridadx2), (x3,prioridadx3), ... }
	union c1 (ponerPQ x prioridadx c2) = union {(x1,prioridadx1), (x2,prioridadx2), (x3,prioridadx3), ...} {(y1,prioridady1), (y2,prioridady2), (y3,prioridady3), ... 
									   = c1 U {(z,prioridadz) (E) c2 / prioridadz (/E) {prioridadx1, prioridadx2 , ...} }

	-}



{-

--9)	
	
	data AGTree a = Node a [AGTree a]

	{- Principio De Inducciòn:

	Dada una propiedad P sobre elementos de AGTree, para probar que para todo t::AGTree, resulta P(t) vàlida:
		. Probamos que P(Node a []) es válida.
		. Probamos que si P(Node a xi), para i = 1, ... , k / xi es elementos de xs, entonces P(Node a xs) es válida.

	-}


--13)

	Definiendo los tipos de datos utilizados tenemos,

	type Rank = Int
	data Heap a = E | N Rank a (Heap a) (Heap a)

	rank, se encarga de devolver el rango

	rank :: Heap a -> Rank
	rank E = 0
	rank (N r _ _ _) = r

	makeH, preserva el invariante leftist

	makeH x a b = if rank a >= rank b then N (rank b + 1) x a b
									  else N (rank a + 1) x b a 


	merge :: Ord a => Heap a -> Heap a -> Heap a

	merge h1 E = h1
	merge E h2 = h2
	merge h1@(N _ x a1 b1) h2@(N _ y a2 b2) = if x <= y then makeH x a1 (merge b1 h1)
														else makeH y a2 (merge h1 b2)


	Consideremos una propiedad P que se vale sobre los elementos de Heap a, la cual:
		. Probamos que P(E) es válida.
		. Si P(a1) y P(a2) resultan válidas entonces P(N rank a a1 a2) también es válida.
	Luego podemos decir que P vale para cualquier elemento de Heap a.

	Queremos probar que para cualquier h1,h2 tales que h1 y h2 son LH, entonces merge h1 h2 tambien es LH.
	Entones tenemos lo siguiente:
	HI : h1 y h2 son LH
	TI : merge h1 h2 es LH

	Aplicando el principio de inducción tenemos.
	Casos bases:

	. h1 = E,
			merge E h2
		= < Definicion de merge .1 >
			h2
		= < HI >
			h2 es LH

	. h2 = E,
			merge h1 E
		= < Definicion de merge .2 >
			h1
		= < HI >
			h1 es LH

	Paso inductivo, donde h1 = N rank1 x a1 b1, y h2 = N rank2 y a2 b2.

		merge h1 h2
	
	= < Definicion de merge .3 >
		
		(*) Caso (x <= y)
		
		= < Definicion de Caso de merge.3 (x <= y) >
		
			makeH x a1 (merge b1 h2)
		
		= < Definicion de makeH >
		
			(**) Caso (rank a1 >= rank (merge b1 h2))
		
			= < Defincion de Caso de makeH (rank a1 >= rank (merge b1 h2)) >
		
				N (rank (merge b1 h2) + 1) x a1 (merge b1 h2)		//Probar que x es menor a cualquier elemento de a1 y b1  // Rango de a1 > b1
		
			= < (1) Como x es raíz de h1, entonces x es menor que cualquier elemento de a1 y b1. Además tenemos que x<=y, implica que x 
				es menor a todos los elementos de h2. Lo cual cumple la condicion de Heap. 
				(2) Como rank a1 >= rank (merge b1 h2), el subarbol izquierdo serà a1, por ende el rango resulta (merge b1 h2)+1. Manteniendo 
				así el invariante. >
			
				N (rank (merge b1 h2) + 1) x a1 (merge b1 h2) es LH.


			(**) Caso (rank a1 < rank (merge b1 h2))
			
			= < Definición de Caso de makeH (rank a1 < rank (merge b1 h2)) >
			
				N (rank a1 + 1) x (merge b1 h2) a1
			
			= < (1) Como x es raíz de h1, entonces x es menor que cualquier elemento de a1 y b1. Además tenemos que x<=y, implica que x 
				es menor a todos los elementos de h2. Lo cual cumple la condicion de Heap.
				(2) Como rank a1 < rank (merge b1 h2), el subarbol izquierdo sera (merge b1 h2), por ende el rango resulta a1+1. Manteniendo 
				así el invariante. >
			
				N (rank a1 + 1) x (merge b1 h2) a1 es LH			
	

		(*) Caso (x > y)
		
		= < Definiciòn de Caso de merge.3 (x > y) >
		
			makeH y a2 (merge h1 b2)
		
		= < Definiciòn de makeH >
		
			(**) Caso (rank a2 >= rank (merge h1 b2))
		
			= < Definicion de Caso de makeH (rank a2 >= rank (merge h1 b2)) >
		
				N (rank (merge h1 b2) + 1) y a2 (merge h1 b2)
		
			= < (1) Como y es raíz de h2, entonces y es menor que cualquier elemento de a2 y b2. Además tenemos que x>y, implica que y 
				es menor a todos los elementos de h1. Lo cual cumple la condicion de Heap.
				(2) Como rank a2 >= rank (merge h1 b2), el subarbol izquierdo sera a2, por ende el rango resulta (merge h1 b2)+1. Manteniendo
				 así el invariante. >
		
				N (rank (merge h1 b2) + 1) y a2 (merge h1 b2) es LH

			(**) Caso (rank a2 < rank (merge h1 b2))
		
			= < Defincion de Caso de makeH (rank a2 < rank (merge h1 b2)) >
		
				N (rank a2 + 1) y (merge h1 b2) a2
		
			= < (1) Como y es raíz de h2, entonces y es menor que cualquier elemento de a2 y b2. Además tenemos que x>y, que implica que y 
				es menor a todos los elementos de h1. Lo cual cumple la condicion de Heap.
				(2) Como rank a2 < rank (merge h1 b2), el subarbol izquierdo sera (merge h1 b2), por ende el rango resulta a2+1. Manteniendo 
				así el invariante. >
		
				N (rank a2 + 1) x (merge h1 b2) a2 es LH
	
	= < Análisis por casos >
	
		merge h1 h2 es LH

-}
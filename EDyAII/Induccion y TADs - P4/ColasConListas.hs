module Cola where

import Data.List
import Data.Char


type Cola a = [a]

vaciaQeue :: Cola a
vacia = [ ]

ponerQeue :: a -> Cola a -> Cola a
poner x ys = x:ys

primeroQeue :: Cola a -> a
primero xs = last xs

sacarQeue :: Cola a -> Cola a
sacar [ ] = [ ]
sacar xs = init xs

esVaciaQeue :: Cola a -> Bool
esVacia xs = null xs

--------------------------------------------------

type ColaDL a = ([a],[a])

vaciaQDL :: ColaDL a
vaciaQDL = ([ ],[ ])

ponerQDL :: a -> ColaDL a -> ColaDL a
ponerQDL x (xs,ys) = validarQDL(xs,x:ys)

primeroQDL :: ColaDL a -> a
primeroQDL ((x:xs),ys) = x

sacarQDL :: ColaDL a -> ColaDL a
sacarQDL ((x:xs),ys) = validarQDL(xs,ys)

esVaciaQDL :: ColaDL a -> Bool
esVaciaQDL (xs,ys) = null xs

validarQDL :: ColaDL a -> ColaDL a
validarQDL (xs,ys) = if esVacia xs then (reverse ys,[ ])
				else (xs,ys)


{- Costos de Implementación:

	- WvaciaQDL = O(1)
	- WesVaciaQDL = O(1)
	- WponerQDL = O(1)
	- WprimeroQDL = O(1)
	- WsacarQDL = O(|xs|), O(1)(AMORTIZADO)

}
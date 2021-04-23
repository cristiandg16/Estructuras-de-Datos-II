module Par ((|||)) where

--import Control.Parallel

infix 1 ||| --le da priordad al operador (de 1 a 9 donde 9 es mas alto)

(|||)   ::   a -> b -> (a,b)
a ||| b = (a,b)

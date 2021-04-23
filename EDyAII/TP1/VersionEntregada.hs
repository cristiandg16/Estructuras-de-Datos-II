{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module TP1 where

data TTree k v = Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v)
               | Leaf k v
               | E  deriving Show


--1. search: devuelve el valor asociado a la clave pasada como argumento.

search                               :: Ord k => [k] -> TTree k v -> Maybe v

search (x:xs) E                      = Nothing

search [x] (Leaf key value)          | x == key  = Just value 
                                     | otherwise = Nothing

search (x:xs) (Leaf key value)       = Nothing

search [x] (Node key value l m r)    | x == key  = value
                                     | x > key   = search [x] r
                                     | otherwise = search [x] l

search (x:xs) (Node key value l m r) | x == key  = search xs m 
                                     | x > key   = search (x:xs) r
                                     | otherwise = search (x:xs) l


--2. insert: agrega un par (clave, valor) a un árbol. Si la clave ya esta en el arbol, actualiza su valor.

insert                                        :: Ord k => [k] -> v -> TTree k v -> TTree k v

insert [x] newValue E                         = Leaf x newValue

insert (x:xs) newValue E                      = Node x Nothing E (insert xs newValue E) E

insert [x] newValue (Leaf key value)          | x == key  = (Leaf key newValue)
                                              | x > key   = (Node key (Just value) E E (Leaf x newValue))
                                              | otherwise = (Node key (Just value) (Leaf x newValue) E E)

insert (x:xs) newValue (Leaf key value)       | x == key  = (Node key (Just value) E (insert xs newValue E) E)
                                              | x > key   = (Node key (Just value) E E (insert (x:xs) newValue E))
                                              | otherwise = (Node key (Just value) (insert (x:xs) newValue E) E E)

insert [x] newValue (Node key value l m r)    | x == key  = (Node x (Just newValue) l m r)
                                              | x > key   = (Node key value l m (insert [x] newValue r))
                                              | otherwise = (Node key value (insert [x] newValue l) m r)

insert (x:xs) newValue (Node key value l m r) | x == key  = (Node key value l (insert xs newValue m) r)
                                              | x > key   = (Node key value l m (insert (x:xs) newValue r))
                                              | otherwise = (Node key value (insert (x:xs) newValue l) m r)


--3. delete: elimina una clave y el valor asociada a ésta en un árbol.

delete                               :: Ord k => [k] -> TTree k v -> TTree k v

delete (x:xs) E                      = E

delete [x] (Leaf key value)          | x == key  = E
                                     | otherwise = (Leaf key value)

delete (x:xs) (Leaf key value)       = (Leaf key value)

delete [x] (Node key value l m r)    | x == key  = (Node key Nothing l m r)
                                     | x > key   = (Node key value l m (delete [x] r))
                                     | otherwise = (Node key value (delete [x] l) m r)

delete (x:xs) (Node key value l m r) | x == key  = (Node key value l (delete xs m) r)
                                     | x > key   = (Node key value l m (delete (x:xs) r))
                                     | otherwise = (Node key value (delete (x:xs) l) m r)


--4. keys: dado un árbol, devuelve una lista ordenada con las claves del mismo.

keys                          :: TTree k v -> [[k]]
keys E                        = [ ]
keys (Leaf key value)         = [[key]]
keys (Node key Nothing l m r) = map reverse ((keys2 l [ ]) ++ (keys2 m [key]) ++ (keys2 r [ ]))
keys (Node key _ l m r)       = map reverse ((keys2 l [ ]) ++ [[key]] ++ (keys2 m) ++ (keys2 r [ ]) ) --nos falto el keys2 en m

--keys2: función auxiliar para keys
keys2                              :: TTree k v -> [k] -> [[k]]
keys2 E str                        = [ ]
keys2 (Leaf key value) str         = [key : str]
keys2 (Node key Nothing l m r) str = (keys2 l str) ++ (keys2 m (key:str)) ++ (keys2 r str)
keys2 (Node key _ l m r) str       = (keys2 l str) ++ [key:str] ++ (keys2 m (key:str)) ++ (keys2 r str)


--5.

class Dic k v d | d -> k v where
  vacio    :: d
  insertar :: Ord k => k -> v -> d -> d
  buscar   :: Ord k => k -> d -> Maybe v
  eliminar :: Ord k => k -> d -> d
  claves   :: Ord k => d -> [k]

instance Ord k => Dic [k] v (TTree k v) where
  vacio                      = E
  insertar clave valor arbol = insert clave valor arbol
  buscar clave arbol         = search clave arbol
  eliminar clave arbol       = delete clave arbol
  claves arbol               = keys arbol

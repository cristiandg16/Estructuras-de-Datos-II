module TTree where


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

data TTree k v = Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v)
                | Leaf k v
                | E deriving Show


-- search, devuelve el vlor asociado a la clave pasada como argumento.

searchTTree :: Ord k => [k] -> TTree k v -> Maybe v

searchTTree xs E = Nothing
searchTTree [x] (Leaf key value) = if x == key then Just value else Nothing
searchTTree (x:xs) (Leaf key value) = Nothing
searchTTree [x] (Node key value l m r) = if x == key then value
                                    else if x > key then searchTTree [x] r
                                    else searchTTree [x] l
searchTTree (x:xs) (Node key value l m r) = if x == key then searchTTree xs m 
                                       else if x > key then searchTTree (x:xs) r
                                       else searchTTree (x:xs) l

-- insert, agregar un par (clave,valor) a un arbol. Si la clave ya esta en el arbol, actualiza su valor.

insertTTree :: Ord k => [k] -> v -> TTree k v -> TTree k v

insertTTree [x] newValue E = Leaf x newValue -- Node [x] newValue E E E
insertTTree (x:xs) newValue E = Node x Nothing E (insertTTree xs newValue E) E
insertTTree [x] newValue (Leaf key value) = if x == key then (Leaf key newValue)
                                            else if x > key then (Node key (Just value) E E (Leaf x newValue))
                                            else (Node key (Just value) (Leaf x newValue) E E)
                                            
insertTTree (x:xs) newValue (Leaf key value) = if x == key then (Node key (Just value) E (insertTTree xs newValue E) E)
                                               else if x > key then (Node key (Just value) E E (insertTTree (x:xs) newValue E))
                                               else (Node key (Just value) (insertTTree (x:xs) newValue E) E E)
                                               
insertTTree [x] newValue (Node key value l m r) = if x == key then (Node x (Just newValue) l m r)
                                            else if x > key then (Node key value l m (insertTTree [x] newValue r))
                                            else (Node key value (insertTTree [x] newValue l) m r)
                                            
insertTTree (x:xs) newValue (Node key value l m r) = if x==key then (Node key value l (insertTTree xs newValue m) r)
                                            else if x > key then (Node key value l m (insertTTree (x:xs) newValue r))
                                            else (Node key value (insertTTree (x:xs) newValue l) m r)


-- delete, elimina una clave y el valor asociado a esta en un arbol.
deleteTTree :: Ord k => [k] -> TTree k v -> TTree k v
deleteTTree _ E = E

deleteTTree [x] hojita@(Leaf key _) = if x == key then E else hojita

deleteTTree [x] (Node key v l m r) = if x == key then (Node key Nothing l m r)
                                    else if x > key then (Node key v l m (deleteTTree [x] r))
                                    else (Node key v (deleteTTree [x] l) m r)

deleteTTree (x:xs) hojita@(Leaf key value) = hojita

deleteTTree (x:xs) (Node key value l m r) = if x == key then (Node key value l (deleteTTree xs m) r)
                                            else if x > key then (Node key value l m (deleteTTree (x:xs) r))
                                            else (Node key value (deleteTTree (x:xs) l) m r)

-- la clave de esta funcion consiste en eliminar los nodos hoja. Ya que si hay una clave parecida, los datos de esta deben
-- permanecer, es decir , el caso en que tengamos (reo,3) y (retos, 1), si queremos eliminar reo, debemos simplemente eliminar
-- la o con la clave 3.

-- determina si un nodo es una hoja.
itsLeaf :: TTree k v -> Bool
itsLeaf E = False
itsLeaf (Leaf key value) = True
itsLeaf (Node key value l m r) = itsLeaf l && itsLeaf m && itsLeaf r


-- keysTTree, dado un arbol devuelve una lista ordenada con las claves del mismo.
keysTTree :: Ord v => TTree k v -> [[k]]
keysTTree E = []
keysTTree (Leaf key value) = [[key]]
keysTTree (Node key value l m r) = if value == Nothing then map reverse ((keys2 l []) ++ (keys2 m [key]) ++ (keys2 r []))
                                   else map reverse ((keys2 l []) ++ [[key]] ++ (keysTTree m) ++ (keys2 r []) )



keys2 :: Ord v => TTree k v -> [k] -> [[k]]
keys2 E str = []
keys2 (Leaf key value) str = [key : str]
keys2 (Node key value l m r) str | value == Nothing = (keys2 l str) ++ (keys2 m (key:str)) ++ (keys2 r str)
                                 | otherwise =  (keys2 l str) ++ [key:str] ++ (keys2 m (key:str)) ++ (keys2 r str)

t =  Node 'r' Nothing E (Node 'e' (Just 16) (Node 'a' Nothing E (Leaf 's' 1) E)
                                            (Node 'o' (Just 2) (Leaf 'd' 9)
                                                                E
                                                                (Leaf 's' 4))
                                            E)
                        (Node 's' Nothing E (Node 'i' (Just 4) (Leaf 'e' 8)
                                                                (Leaf 'n' 7)
                                                                    E)
                                            E)

a = insertTTree "reo" 2 E
b = insertTTree "red" 4 a
c = insertTTree "res" 9 b
d = insertTTree "re" 16 c
e = insertTTree "ras" 1 d
f = insertTTree "sin" 7 e 
g = insertTTree "si" 4 f
h = insertTTree "se" 8 g


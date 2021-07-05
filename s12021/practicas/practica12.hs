-- Ejercicio 1) Dada la definición de ExpA:
data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA

-- a. Dar el tipo y definir foldExpA, que expresa el esquema de recursión estructural para la estructura ExpA.

foldExpA:: (Int -> b) -> (b-> b -> b) -> (b-> b -> b) -> ExpA -> b
foldExpA fc fs fp (Cte n)      = fc n
foldExpA fc fs fp (Suma e1 e2) = fs (foldExpA fc fs fp e1) (foldExpA fc fs fp e2) 
foldExpA fc fs fp (Prod e1 e2) = fp (foldExpA fc fs fp e1) (foldExpA fc fs fp e2) 

--b. Resolver las siguientes funciones utilizando foldExpA:
cantidadDeCeros :: ExpA -> Int -- que describe la cantidad de ceros explícitos en la expresión dada.
cantidadDeCeros = foldExpA unoSiEsCero (+) (+)
	where unoSiEsCero x = if x == 0 then 1 else 0

noTieneNegativosExplicitosExpA :: ExpA -> Bool -- que describe si la expresión dada no tiene números negativos de manera explícita.
noTieneNegativosExplicitosExpA = foldExpA esPositivo (&&) (&&)
	where esPositivo x = (x>=0)

simplificarExpA' :: ExpA -> ExpA -- que describe una expresión con el mismo significado que la dada, pero que no tiene sumas del número 0 ni multiplicaciones por 1 o por 0. La resolución debe ser exclusivamente simbólica.
simplificarExpA' = foldExpA Cte simpSuma simpProd

simpSuma:: ExpA -> ExpA -> ExpA 
simpSuma (Cte 0) e2 = e2
simpSuma e1 (Cte 0) = e1
simpSuma e1 e2      = Suma e1 e2

simpProd:: ExpA -> ExpA -> ExpA 
simpProd (Cte 0) e2 = (Cte 0)
simpProd e1 (Cte 0) = (Cte 0)
simpProd (Cte 1) e2 = e2
simpProd e1 (Cte 1) = e1
simpProd e1 e2      = Prod e1 e2

evalExpA' :: ExpA -> Int -- que describe el número que resulta de evaluar la cuenta representada por la expresión aritmética dada.
evalExpA' = foldExpA id (+) (*)

--showExpA :: ExpA -> String -- que describe el string sin espacios y con paréntesis correspondiente a la expresión dada


--c. Demostrar que evalExpA' es equivalente a evalExpA (ejercicio 6.a.i de la práctica 8).
{-
evalExpA::ExpA->Int
evalExpA (Cte n) = n
evalExpA (Suma e1 e2) = evalExpA e1 + evalExpA e2
evalExpA (Prod e1 e2) = evalExpA e1 * evalExpA e2 

	¿evalExpA = evalExpA'?

Por ppio de extensionalidad sea e una ExpA cualquiera
	¿evalExpA e = evalExpA' e?
	
Por inducción en la estructura de e 

- Caso base e = Cte n
	¿evalExpA (Cte n) = evalExpA' (Cte n)?
- Caso ind1 e = (Suma e1 e2)
	TI: ¿evalExpA (Suma e1 e2) = evalExpA' (Suma e1 e2)?
	HI1: evalExpA e1 = evalExpA' e1!
	HI2: evalExpA e2 = evalExpA' e2!
- Caso ind2 e = (Prod e1 e2)
	TI: ¿evalExpA (Prod e1 e2) = evalExpA' (Prod e1 e2)?
	HI1: evalExpA e1 = evalExpA' e1!
	HI2: evalExpA e2 = evalExpA' e2!

Demostración

- Caso base
	evalExpA (Cte n) 
	= evalExpA.1
	n
	
	evalExpA' (Cte n)
	= def evalExpA'
	foldExpA id (+) (*) (Cte n)
	= foldExpA.1
	id (Cte n)
	= def id
	n
	
- Caso ind1
	evalExpA (Suma e1 e2) 
	= evalExpA.2
	evalExpA e1 + evalExpA e2
	
	
	evalExpA' (Suma e1 e2)
	= def evalExpA'
	foldExpA id (+) (*) (Suma e1 e2)
	= foldExpA.2
	(+) (foldExpA id (+) (*) e1) (foldExpA id (+) (*) e2)
	= def evalExpA'
	(+) (evalExpA' e1) (foldExpA id (+) (*) e2)
	= def evalExpA'
	(+) (evalExpA' e1) (evalExpA' e2)
	= HI1 e HI2
	(+) (evalExpA e1) (evalExpA e2)
	= sección operadores
	evalExpA e1 + evalExpA e2
	
	LLEGUÉ
	
- Caso ind2
		Misma idea pero donde dice Suma poner Prod
-}


-- d. Dar el tipo y definir recExpA, que expresa el esquema de recursión primitiva para la estructura ExpA.

recExpA:: (Int -> b) -> (ExpA -> b-> ExpA -> b -> b) -> (ExpA -> b-> ExpA -> b -> b) -> ExpA -> b
recExpA fc fs fp (Cte n)      = fc n
recExpA fc fs fp (Suma e1 e2) = fs e1 (recExpA fc fs fp e1) e2 (recExpA fc fs fp e2) 
recExpA fc fs fp (Prod e1 e2) = fp e1 (recExpA fc fs fp e1) e2 (recExpA fc fs fp e2)

--e. Resolver las siguientes funciones utilizando foldExpA:
cantDeSumaCeros :: ExpA -> Int -- que describe la cantidad de constructores de suma con al menos uno de sus hijos constante cero.
cantDeSumaCeros = recExpA c s p
  where c cn = 0
        s (Cte 0) r1 e2 r2 = 1 + r1 + r2
        s e1 r1 (Cte 0) r2 = 1 + r1 + r2
        s e1 r1 e2 r2      = r1 + r2
        p e1 r1 e2 r2      = r1 + r2


cantDeProdUnos :: ExpA -> Int -- que describe la cantidad de constructores de producto con al menos uno de sus hijos constante uno.
cantDeProdUnos = recExpA c s p
  where c cn = 0
        s e1 r1 e2 r2      = r1 + r2
        p (Cte 1) r1 e2 r2 = 1 + r1 + r2
        p e1 r1 (Cte 1) r2 = 1 + r1 + r2
        p e1 r1 e2 r2      = r1 + r2
		
		
--Ejercicio 2) Dada la definición de EA:
data EA = Const Int | BOp BinOp EA EA
data BinOp = Sum | Mul

--a. Dar el tipo y definir foldEA, que expresa el esquema de recursión estructural para la estructura EA.

foldEA:: (Int -> b) -> (BinOp -> b -> b -> b) -> EA -> b
foldEA fc fb (Const n) = fc n
foldEA fc fb (BOp b ea1 ea2) = fb b (foldEA fc fb ea1) (foldEA fc fb ea1) 

--b. Resolver las siguientes funciones utilizando foldEA:
noTieneNegativosExplicitosEA :: EA -> Bool -- que describe si la expresión dada no tiene números negativos de manera explícita.
noTieneNegativosExplicitosEA = foldEA esPositivo (\b rea1 rea2 -> rea1 && rea2)
 where esPositivo x = (x>=0)

simplificarEA' :: EA -> EA -- que describe una expresión con el mismo significado que la dada, pero que no tiene sumas del número 0 ni multiplicaciones por 1 o por 0. La resolución debe ser exclusivamente simbólica.
simplificarEA' = foldEA Const simpBinOp 

simpBinOp:: BinOp -> EA -> EA -> EA
simpBinOp Sum = simpSumaEA 
simpBinOp Mul = simpMulEA

simpSumaEA::EA -> EA -> EA
simpSumaEA (Const 0) e2 = e2
simpSumaEA e1 (Const 0) = e1
simpSumaEA e1 e2 = BOp Sum e1 e2

simpMulEA::EA -> EA -> EA
simpMulEA (Const 0) e2 = (Const 0)
simpMulEA (Const 1) e2 = e2
simpMulEA e1 (Const 0) = (Const 0)
simpMulEA e1 (Const 1) = e1
simpMulEA e1 e2 = BOp Mul e1 e2

evalEA' :: EA -> Int -- que describe el número que resulta de evaluar la cuenta representada por la expresión aritmética dada.
evalEA' = foldEA id evalBOp

evalBOp::BinOp->Int->Int->Int
evalBOp Sum = (+)
evalBOp Mul = (*)

showEA :: EA -> String -- que describe el string sin espacios y con paréntesis correspondiente a la expresión dada.
showEA = undefined

ea2ExpA' :: EA -> ExpA -- que describe una expresión aritmética representada con el tipo ExpA, cuyo significado es el mismo que la dada.
ea2ExpA' = foldEA Cte binOp2ExpA

binOp2ExpA :: BinOp -> ExpA -> ExpA -> ExpA
binOp2ExpA Sum = Suma
binOp2ExpA Mul = Prod

data ABTree a b = Leaf b | Node a (ABTree a b) (ABTree a b)
ea2Arbol' :: EA -> ABTree BinOp Int -- que describe la representación como elemento del tipo ABTree BinOp Int de la expresión aritmética dada.
ea2Arbol' = foldEA Leaf (\ b e1 e2 -> Node b e1 e2)


{-
Demostrar que evalEA' es equivalente a evalEA (ejercicio 1.a.i de lapráctica 9).

evalEA::EA->Int	
evalEA (Const n) = n
evalEA (BOp b e1 e2) = evalBOp b (evalEA e1) (evalEA e2)

evalBOp::BinOp->Int->Int->Int
evalBOp Sum = (+)
evalBOp Mul = (*)


¿evalEA' = evalEA ?

- Caso base ea = Const n
	evalEA' (Const n) = evalEA (Const n) 
- Caso ind ea = BOp b e1 e2
	TI: evalEA' (BOp b e1 e2) = evalEA (BOp b e1 e2)
	HI1: evalEA' e1 = evalEA e1
	HI1: evalEA' e2 = evalEA e2

Demostración

- Caso base
	evalEA' (Const n)
	= def evalEA'
	foldEA id evalBOp (Const n)
	= foldEA.1
	id n 
	= def id
	n 
	= evalEA.1
	evalEA (Const n)
	
- Caso ind
	evalEA' (BOp b e1 e2) 
	= def evalEA'
	foldEA id evalBOp (BOp b e1 e2) 
	= foldEA.2
	evalBOp b (foldEA id evalBOp e1) (foldEA id evalBOp e2)
	= def evalEA
	evalBOp b (evalEA' e1) (foldEA id evalBOp e2)
	= def evalEA
	evalBOp b (evalEA' e1) (evalEA' e2)
	= HI1
	evalBOp b (evalEA e1) (evalEA' e2)
	= HI2
	evalBOp b (evalEA e1) (evalEA e2)
	= def evalEA
	evalEA (BOp b e1 e2)

-}

-- Ejercicio 3) Dada la definición de Tree:
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

--Dar el tipo y definir la función foldT, que expresa el esquema de recursión estructural para la estructura Tree.
foldT:: b -> (a -> b -> b -> b) -> Tree a -> b
foldT fe fn EmptyT          = fe 
foldT fe fn (NodeT x t1 t2) = fn x (foldT fe fn t1) (foldT fe fn t2)

-- b. Definir las siguientes funciones utilizando foldT:

mapT :: (a -> b) -> Tree a -> Tree b
mapT f = foldT EmptyT (NodeT . f)

sumT :: Tree Int -> Int
sumT = foldT 0 z
 where z x r1 r2 = x + r1 + r2
 
sizeT :: Tree a -> Int
sizeT = foldT 0 z
 where z x r1 r2 = 1 + r1 + r2 

heightT :: Tree a -> Int
heightT = foldT 0 z
 where z x r1 r2 = 1 + max r1 r2 

preOrder :: Tree a -> [a]
preOrder = foldT [] z
 where z x r1 r2 = x : r1 ++ r2

inOrder :: Tree a -> [a]
inOrder = foldT [] z
 where z x r1 r2 = r1 ++ [x] ++ r2
 
postOrder :: Tree a -> [a]
--postOrder = foldT [] (\x r1 r2 -> r1 ++ r2 ++ [x])
postOrder = foldT [] z
 where z x r1 r2 = r1 ++ r2 ++[x]

mirrorT :: Tree a -> Tree a
mirrorT = foldT EmptyT z
 where z x r1 r2 = NodeT x r2 r1

countByT :: (a -> Bool) -> Tree a -> Int
countByT f = foldT 0 z
 where z x r1 r2 = if f x then 1 else 0 + r1 + r2

partitionT :: (a -> Bool) -> Tree a -> ([a], [a])
partitionT f = foldT ([],[]) z
 where z x (ri1, ri2) (rd1, rd2) = if f x then (x : ri1++rd1, ri2++rd2 ) else ( ri1++rd1, x : ri2++rd2 )

{-zipWithT f EmptyT EmptyT = EmptyT
zipWithT f EmptyT (NodeT x ri rd) = EmptyT
zipWithT f (NodeT x ri rd) EmptyT = EmptyT
zipWithT f (NodeT x rxi rxd) (NodeT y ryi ryd) = NodeT (f x y) (zipWithT f rxi ryi) (zipWithT f rxd ryd)
-}

zipWithT :: (a->b->c) -> Tree a -> Tree b -> Tree c
zipWithT f t1 t2 = foldT (const EmptyT) z t1 t2
 where z x ri rd EmptyT = EmptyT
       z x ri rd (NodeT y ryi ryd) = NodeT (f x y) (ri ryi) (rd ryd)

caminoMasLargo :: Tree a -> [a]
caminoMasLargo = foldT [] z 
 where z x ri rd = x: maxListas ri rd

maxListas xs ys = if length xs > length ys then xs else ys
 
{-todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos = foldT [[]] z
 where z x ri rd = agregarAListasDeListas x ri rd 

agregarAListasDeListas::a -> [[a]] -> [[a]] -> [[a]]
agregarAListasDeListas x xss yss = agregarAListas x xss ++ agregarAListas x yss

agregarAListas::a -> [[a]] -> [[a]]
agregarAListas x [] = [[]]
agregarAListas x (xs:[]) = [x:xs]
agregarAListas x (xs:xss) = (x:xs): agregarAListas x xss-}
 
 
listarPorNiveles :: Tree a -> [[a]]
listarPorNiveles = foldT [] (\x ri rd -> [x] : concatPerLevel ri rd)

concatPerLevel::[[a]]->[[a]]->[[a]]
--concatPerLevel [] []        = []
--concatPerLevel xss []       = xss 
--concatPerLevel [] yss       = yss
--concatPerLevel (xs:xss) yss = xs : concatPerLevel xss yss
concatPerLevel xs ys = recr (\ys -> ys) g xs ys    --como xs e ys se pasan como se reciben, se pueden sacar
 where g x xs r []     = x : xs
       g x xs r (y:ys) = (x++y) : r ys

{-nivelN :: Tree a -> Int -> [a]
nivelN EmptyT n = []
nivelN (NodeT x ti td) 0 = [x]
nivelN (NodeT x ti td) n = nivelN ti (n-1) ++ nivelN td (n-1)-}
		
nivelN :: Tree a -> Int -> [a]
nivelN t n = foldT g z t n
 where g t         = []
       z x rti rtd 0 = [x]
       z x rti rtd n = rti (n-1) ++ rtd (n-1)

-- c. Dar el tipo y definir la función recT, que expresa el esquema de recursión primitiva para la estructura Tree.

recT:: b -> (a -> Tree a -> b -> Tree a -> b -> b) -> Tree a -> b
recT fe fn EmptyT          = fe 
recT fe fn (NodeT x t1 t2) = fn x t1 (recT fe fn t1) t2 (recT fe fn t2)

-- d. Definir las siguientes funciones utilizando recT:

{-todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos = foldT [[]] z
 where z x ri rd = map (x:) (ri++rd) 
 tiene duplicados porque queda map (x:) ([[]], [[]]) 
 cuando es NodeT x EmptyT EmptyT
-}

{-
insertT e EmptyT = NodeT e EmptyT EmptyT
insertT e (NodeT x ti td) = if e < x then NodeT x (insertT e ti) td else NodeT x ti (insertT e td)
-}

insertT :: Ord a =>  a -> Tree a -> Tree a -- que describe el árbol resultante de insertar el elemento dado en el árbol dado, teniendo en cuenta invariantes de BST.
insertT e t = recT EmptyT g t 
  where g x ti rti td rtd = 
         if e < x 
             then NodeT x (NodeT e rti EmptyT) td 
             else NodeT x ti (NodeT e rtd EmptyT)

caminoHasta :: Eq a => a -> Tree a -> [a] -- que describe el camino hasta el elemento dado en el árbol dado. 	Precondición: existe el elemento en el árbol.
caminoHasta = undefined


{-
e. Demostrar las siguientes propiedades utilizando las definiciones anteriores:

i. para todo f. sizeT . mapT f = sizeT

Por ppio de extensionalidad sea t un Tree a cualquiera
	sizeT . mapT f) t = sizeT t?
	
Por def (.)
	sizeT (mapT f t) = sizeT t
	
Por ppio de inducción en la estructura de t
- Caso base t = EmptyT
	sizeT (mapT f EmptyT) = sizeT EmptyT
- Caso ind  t = NodeT x t1 t2
	TI: sizeT (mapT f (NodeT x t1 t2)) = sizeT (NodeT x t1 t2)
	HI1: sizeT (mapT f t1) = sizeT t1
	HI2: sizeT (mapT f t2) = sizeT t2
	
Demostración
- Caso base 
	sizeT (mapT f EmptyT) 
	= def mapT
	sizeT (foldT EmptyT (NodeT . f) EmptyT)
	= foldT.1
	sizeT EmptyT
	
- Caso ind
	sizeT (mapT f (NodeT x t1 t2)) 
	= def mapT
	sizeT (foldT EmptyT (NodeT . f) (NodeT x t1 t2))
	= foldT.2
	sizeT ((NodeT . f) x (foldT EmptyT (NodeT . f) t1) (foldT EmptyT (NodeT . f) t2))
	= def mapT
	sizeT ((NodeT . f) x (mapT f t1)  (foldT EmptyT (NodeT . f) t2))
	= def mapT
	sizeT ((NodeT . f) x (mapT f t1)  (mapT f t2))
	= def (.)
	sizeT (NodeT (f x) (mapT f t1)  (mapT f t2))
	= def sizeT
	foldT 0 z (NodeT (f x) (mapT f t1)  (mapT f t2))
	= foldT.2
	z x (foldT 0 z (mapT f t1)) (foldT 0 z (mapT f t2))
	= def sizeT
	z x (sizeT (mapT f t1)) (foldT 0 z (mapT f t2))
	= def sizeT
	z x (sizeT (mapT f t1)) (sizeT (mapT f t2))
	= HI1
	z x (sizeT t1) (sizeT (mapT f t2))
	= HI2
	z x (sizeT t1) (sizeT t2)
	
	
	sizeT (NodeT x t1 t2)
	= def sizeT
	foldT 0 z (NodeT x t1 t2)
	= foldT.2
	z x (foldT 0 z t1) (foldT 0 z t2)
	= def sizeT
	z x (sizeT t1) (foldT 0 z t2)
	= def sizeT
	z x (sizeT t1) (sizeT t2)


ii. para todo f. para todo g. mapT f . mapT g = mapT (f . g)
iii. foldT EmptyT NodeT = id

-}


--Ejercicio 6) Dadas las siguientes definiciones para representar mapas con diferentes puntos de interés que pueden presentar objetos:

data Dir = Lt | Rt | Straight
data Mapa a = Cofre [a] | Nada (Mapa a) | Bifurcacion [a] (Mapa a) (Mapa a)

-- a. Dar el tipo y definir foldM y recM, que expresan los esquemas de recursión estructural y primitiva, respectivamente, para la estructura Mapa.

foldM:: ([a] -> b) -> (b -> b) -> ([a]-> b-> b-> b) -> Mapa a -> b
foldM fc fn fb (Cofre xs)             = fc xs
foldM fc fn fb (Nada m)               = fn (foldM fc fn fb m)
foldM fc fn fb (Bifurcacion xs mi md) = fb xs (foldM fc fn fb mi) (foldM fc fn fb md)

recM:: ([a] -> b) -> (Mapa a -> b -> b) -> ([a]-> Mapa a -> b-> Mapa a -> b-> b) -> Mapa a -> b
recM fc fn fb (Cofre xs)             = fc xs
recM fc fn fb (Nada m)               = fn m (recM fc fn fb m)
recM fc fn fb (Bifurcacion xs mi md) = fb xs mi (recM fc fn fb mi) md (recM fc fn fb md)

-- b. Definir las siguientes funciones sin utilizar recursión explícita:

objects :: Mapa a -> [a] -- que describe la lista de todos los objetos presentes en el mapa dado.
objects = foldM id id z
 where z xs rmi rmd = xs ++ rmi ++ rmd

mapM :: (a -> b) -> Mapa a -> Mapa b -- que transforma los objetos del mapa dado aplicando la función dada.
mapM f = foldM fc fn fb
 where fc xs = Cofre (map f xs)
       fn m = Nada m
       fb xs rmi rmd = Bifurcacion (map f xs) rmi rmd

has :: (a -> Bool) -> Mapa a -> Bool -- que indica si existe algún objeto que cumpla con la condición dada en el mapa dado.
has f = foldM (any f) id (\xs rmi rmd -> (any f xs) || rmi || rmd) 

hasObjectAt :: (a->Bool) -> Mapa a -> [Dir] -> Bool -- que indica si un objeto al final del camino dado cumple con la condición dada en el mapa dado.
hasObjectAt f = foldM c n b
 where c xs []                 = any f xs
       c xs (d:ds)             = False
       n m  (Straight:ds)      = m ds
       n m  ds                 = False
       b xs rmi rmd []         = any f xs
       b xs rmi rmd (Rt:ds) = rmd ds
       b xs rmi rmd (Lt:ds)  = rmi ds 
       b xs rmi rmd (d:ds)     = False

longestPath :: Mapa a -> [Dir] -- que describe el camino más largo en el mapa dado.
longestPath = foldM (const []) c b 
 where c rm = Straight:rm
       b xs rmi rmd = if length rmi > length rmd then Lt:rmi else Rt:rmd

objectsOfLongestPath :: Mapa a -> [a] -- que describe la lista con los objetos presentes en el camino más largo del mapa dado.
objectsOfLongestPath = recM id n b
 where n m rm             = rm
       b xs mi rmi md rmd = xs ++ (if longestMapPath mi md then rmi else rmd)

longestMapPath :: Mapa a -> Mapa a -> Bool
longestMapPath m1 m2 = length (longestPath m1) >= length (longestPath m2)

allPaths :: Mapa a -> [[Dir]] -- que describe la lista con todos los caminos del mapa dado.
allPaths = foldM (const [[]]) (addDirToAll Straight) b
 where b xs rmi rmd = addDirToAll Lt rmi ++ addDirToAll Rt rmd
 
addDirToAll :: Dir -> [[Dir]] -> [[Dir]]
addDirToAll d []      = [[d]]
addDirToAll d (ds:dss) = addAll d ds dss

addAll :: Dir -> [Dir] -> [[Dir]] -> [[Dir]]
addAll d xs xss = [d:xs] ++ xss

objectsPerLevel :: Mapa a -> [[a]] -- que describe la lista con todos los objetos por niveles del mapa dado.
objectsPerLevel = foldM c n b
 where c xs         = [xs]
       n rm         = rm
       b xs rmi rmd = xs: appendLevels rmi rmd 
	   
appendLevels::[[a]]->[[a]]->[[a]]
appendLevels [] yss = yss
appendLevels xss [] = xss 
appendLevels (xs:xss) (ys:yss) =  (xs ++ ys) : appendLevels xss yss

{-
c. Demostrar la siguiente propiedad:
para todo x. has (==x) = any (elem x) . objectsPerLevel

	has (==x) = any (elem x) . objectsPerLevel?
	
Por ppio de extensionalidad sea m un Mapa a cualquiera
	has (==x) m = (any (elem x) . objectsPerLevel) m?
	
Por def (.)
	has (==x) m = any (elem x) (objectsPerLevel m)?

Por inducción en la estructura de m
- Caso base m = (Cofre xs)
	has (==x) (Cofre xs) = any (elem x) (objectsPerLevel (Cofre xs))?
- Caso ind1 m = (Nada m')
	TI: has (==x) (Nada m') = any (elem x) (objectsPerLevel (Nada m'))?
	HI: has (==x) m' = any (elem x) (objectsPerLevel m')!
- Caso ind2 m = (Bifurcacion xs mi md)
	TI: has (==x) (Bifurcacion xs mi md) = any (elem x) (objectsPerLevel (Bifurcacion xs mi md))?
	HI1: has (==x) mi = any (elem x) (objectsPerLevel mi!
	HI2: has (==x) md = any (elem x) (objectsPerLevel md!

Demostración
- Caso base
	has (==x) (Cofre xs) 
	= def has
	foldM (any (==x)) id (\xs rmi rmd -> (any (==x) xs) || rmi || rmd) (Cofre xs)
	= foldM.1
	(any (==x)) (Cofre xs)
	= aplicacion
	any (==x) xs
	
	any (elem x) (objectsPerLevel (Cofre xs))
	= def objectsPerLevel
	any (elem x) (foldM c n b (Cofre xs))
	= foldM.1
	any (elem x) (c xs)
	= def c
	any (elem x) [xs]
	= def any
	elem x [] || any (elem x) xs
	= elem.1
	False || any (elem x) xs
	= def OR
	any (elem x) xs
	= lema any-elem
	any (==x) xs
	
	LLEGUÉ
	
- Caso ind1
	has (==x) (Nada m') 
	= def has
	foldM (any (==x)) id (\xs rmi rmd -> (any (==x) xs) || rmi || rmd) (Nada m')
	= foldM.2
	id (foldM (any (==x)) id (\xs rmi rmd -> (any (==x) xs) || rmi || rmd) m')
	= def has
	id (has (any (==x)) m')
	= HI
	id (any (elem x) (objectsPerLevel m'))
	= def id
	any (elem x) (objectsPerLevel m')
	
	
	any (elem x) (objectsPerLevel (Nada m'))
	= def objectsPerLevel
	any (elem x) (foldM c n b (Nada m'))
	= foldM.2
	any (elem x) (n (foldM c n b m'))
	= def objectsPerLevel m'
	any (elem x) (n (objectsPerLevel m'))
	= def n
	any (elem x) (objectsPerLevel m')
	
	LLEGUÉ
	
- Caso ind2
	has (==x) (Bifurcacion xs mi md) 
	= def has
	foldM (any (==x)) id (\xs rmi rmd -> (any (==x) xs) || rmi || rmd) (Bifurcacion xs mi md)
	= foldM.3
	(\xs rmi rmd -> (any (==x) xs) || rmi || rmd) 
		xs (foldM any (==x)) id (\xs rmi rmd -> (any (==x) xs) || rmi || rmd) mi) 
		   (foldM any (==x)) id (\xs rmi rmd -> (any (==x) xs) || rmi || rmd) md)
	= beta reducción
	(any (==x) xs) || (foldM any (==x)) id (\xs rmi rmd -> (any (==x) xs) || rmi || rmd) mi)
	               || (foldM any (==x)) id (\xs rmi rmd -> (any (==x) xs) || rmi || rmd) md)
	
	= def has
	(any (==x) xs) || has any (==x)) mi
	               || (foldM any (==x)) id (\xs rmi rmd -> (any (==x) xs) || rmi || rmd) md)
	= def has
	any (==x) xs || has (==x)) mi || has(==x)) md
	= HI1 e HI2
	any (==x) xs || any (elem x) (objectsPerLevel mi || any (elem x) (objectsPerLevel md)
	= lema any-elem
	any (elem x) || any (elem x) (objectsPerLevel mi || any (elem x) (objectsPerLevel md)
	
	any (elem x) (objectsPerLevel (Bifurcacion xs mi md))
	= def objectsPerLevel
	any (elem x) (foldM c n b (Bifurcacion xs mi md))
	= foldM.3
	any (elem x) (b xs (foldM c n b mi) (foldM c n b md))
	= def objectsPerLevel
	any (elem x) (b xs (objectsPerLevel mi)  (foldM c n b md))
	= def objectsPerLevel
	any (elem x) (b xs (objectsPerLevel mi)  (objectsPerLevel md))	
	= def b 
	any (elem x) (xs : appendLevels (objectsPerLevel mi) (objectsPerLevel md))
	= def any
	elem x xs || any (elem x) (appendLevels (objectsPerLevel mi) (objectsPerLevel md))
	
-}


--Ejercicio 7) Dada la siguiente definición para representar árboles generales:
data GTree a = GNode a [GTree a]

--a. Dar tipo y definir las tres versiones de foldGT y recGT, que expresan los esquemas de recursión estructural y primitiva, respectivamente, para la estructura GTree. ATENCIÓN: recordar que no siguen la secuencia dada para tipos recurslvos con recursión directa, porque la recursión en GTree NO es directa. Estas versiones deben tener en cuenta el esquema de recursión sobre listas correspondiente, como se trató en clase teórica.

foldGT0 :: (a -> [b] -> b) -> GTree a -> b
foldGT0 f (GNode e xs) = f e (map (foldGT0 f) xs)

foldGT1 :: (a -> c -> b) -> (b -> c -> c) -> c -> GTree a -> b
foldGT1 g f z (GNode e xs) = g e (foldr f z (map (foldGT1 g f z) xs))

foldGT :: (a -> c -> b) -> ([b] -> c ) -> GTree a -> b
foldGT g f (GNode e xs) = g e (f (map (foldGT g f) xs))

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr f g []     = f
recr f g (x:xs) = g x xs (recr f g xs)

recGT0 :: (a -> [GTree a] -> [b] -> b) -> GTree a -> b
recGT0 f (GNode e xs) = f e xs (map (recGT0 f) xs)

--recGT1 :: (a -> c -> b) -> ([GTree a] -> b -> c -> c) -> c -> GTree a -> b
--recGT1 g f z (GNode e xs) = g e (recr f z (map (recGT1 g f z) xs))

recGT :: (a -> c -> [GTree a] -> b) -> ([b] -> c ) -> GTree a -> b
recGT g f (GNode e xs) = g e (f (map (recGT g f) xs)) xs

--b. Definir las siguientes funciones sin utilizar recursión explícita:

mapGT :: (a -> b) -> GTree a -> GTree b
-- mapGT f = foldGT0 (\x rts -> GNode (f x) rts)
-- mapGT f = foldGT1 (\x rts -> GNode (f x) rts) (:) []
mapGT f = foldGT (\x rts -> GNode (f x) rts) id

sumGT :: GTree Int -> Int
-- sumGT = foldGT0 (\x rts -> x + rts) 
-- sumGT = foldGT1 (+) (+) 0
sumGT = foldGT (+) sum

sizeGT :: GTree a -> Int
-- sizeGT = foldGT0 (\x rts -> 1 + rts) 
-- sizeGT = foldGT1 (\x rts -> 1 + rts) (\x ts -> 1 + ts) 0
sizeGT = foldGT (\x rts -> 1 + rts) sum

gtree = GNode 1 [GNode 2 [], GNode 3[], GNode 4 [GNode 5 [], GNode 6[]]]

heightGT :: GTree a -> Int
--heightGT (GNode e ts) = 1 + maxOr 0 (map heightGT ts)
heightGT = recGT z g
 where z x tc ts = 1 + tc
       g [] = 0
       g (x:xs) = maximum xs
	

{-
preOrderGT :: GTree a -> [a]
inOrderGT :: GTree a -> [a]
postOrderGT :: GTree a -> [a]
-}
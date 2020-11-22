data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA deriving Show

foldExpA::(Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
foldExpA fc fs fp (Cte n)= fc n
foldExpA fc fs fp (Suma e1 e2)= fs (foldExpA fc fs fp e1) (foldExpA fc fs fp e2)
foldExpA fc fs fp (Prod e1 e2)= fp (foldExpA fc fs fp e1) (foldExpA fc fs fp e2)

cantidadDeCeros :: ExpA -> Int
cantidadDeCeros = foldExpA unoSiEsCero (+) (+)

unoSiEsCero:: Int -> Int
unoSiEsCero 0 = 1
unoSiEsCero _ = 0 

noTieneNegativosExplicitosExpA:: ExpA -> Bool
noTieneNegativosExplicitosExpA = foldExpA (not . esNegativo) (&&) (&&)

esNegativo::Int -> Bool
esNegativo n = n < 0

simplificarExpA' :: ExpA -> ExpA
simplificarExpA' = foldExpA Cte simpSumaExpA' simpProdExpA'

simpSumaExpA'::ExpA-> ExpA -> ExpA
simpSumaExpA' (Cte 0) e2 = e2
simpSumaExpA' e1 (Cte 0) = e1

simpProdExpA'::ExpA-> ExpA -> ExpA
simpProdExpA' (Cte 0) e2 = (Cte 0) 
simpProdExpA' (Cte 1) e2 = e2
simpProdExpA' e1 (Cte 0) = (Cte 0)
simpProdExpA' e1 (Cte 1) = e1

evalExpA' :: ExpA -> Int
evalExpA' = foldExpA id (+) (*)

--showExpA::ExpA->String​
--showExpA = foldExpA

{-
Demostraciones

evalExpA::ExpA->Int
evalExpA (Cte n) = n
evalExpA (Suma e1 e2) = evalExpA e1 + evalExpA e2
evalExpA (Prod e1 e2) = evalExpA e1 * evalExpA e2

¿evalExpA' = ​evalExpA?

¿evalExpA' e = ​evalExpA e?

- Caso base e = Cte n 
	¿evalExpA' (Cte n) = ​evalExpA (Cte n )?
- Caso ind1 e = Suma e1 e2
	TI: ¿evalExpA' (Suma e1 e2) = ​evalExpA (Suma e1 e2)?
	HI1:¡evalExpA' e1 = ​evalExpA e1!
	HI2:¡evalExpA' e2 = ​evalExpA e2!
- Caso ind2 e = Prod e1 e2
	TI: ¿evalExpA' (Prod e1 e2) = ​evalExpA (Prod e1 e2)?
	HI1:¡evalExpA' e1 = ​evalExpA e1!
	HI2:¡evalExpA' e2 = ​evalExpA e2!

Demostración

- Caso base
	evalExpA' (Cte n)
	= def evalExpA'
	foldExpA id (+) (*) (Cte n)
	= foldExpA.1
	id n
	= def id
	n 
	
	evalExpA (Cte n)
	= evalExpA.1
	n
	
- Caso ind1
	evalExpA' (Suma e1 e2) 
	= def evalExpA'
	foldExpA id (+) (*) (Suma e1 e2)
	= foldExpA.2
	(+) (foldExpA id (+) (*) e1) (foldExpA id (+) (*) e2)
	= sección operadores
	foldExpA id (+) (*) e1) + (foldExpA id (+) (*) e2)
	= def evalExpA'
	evalExpA' e1 + (foldExpA id (+) (*) e2)
	= def evalExpA'
	evalExpA' e1 + evalExpA' e2

	​evalExpA (Suma e1 e2)
	=evalExpA.2
	evalExpA e1 + evalExpA e2
	= HI1
	evalExpA' e1 + evalExpA e2
	= HI2
	evalExpA' e1 + evalExpA' e2

- Caso ind2
	Es igual a caso ind1 solo que donde dice + hay que poner *
	
-}

recExpA::(Int->b) -> (ExpA -> b -> ExpA -> b-> b) -> (ExpA -> b -> ExpA -> b-> b) -> ExpA -> b
recExpA fc fs fp (Cte n)= fc n
recExpA fc fs fp (Suma e1 e2)= fs e1 (recExpA fc fs fp e1) e2 (recExpA fc fs fp e2)
recExpA fc fs fp (Prod e1 e2)= fp e1 (recExpA fc fs fp e1) e2 (recExpA fc fs fp e2)

--cantDeSumaCeros::ExpA->Int
--cantDeSumaCeros = foldExpA ? ? ?

--cantDeProdUnos::ExpA->Int​

-----------------------
--Ej 2

data EA = Const Int | BOp BinOp EA EA     
data BinOp = Sum | Mul

foldEA::(Int -> b) -> (BinOp -> b -> b -> b) -> EA -> b
foldEA fc fb (Const n) = fc n
foldEA fc fb (BOp b e1 e2) = fb b (foldEA fc fb e1) (foldEA fc fb e1)

noTieneNegativosExplicitosEA::EA->Bool
noTieneNegativosExplicitosEA = foldEA (not . esNegativo) (\b re1 re2 -> re1 && re2)
-- no sirve para los casos donde el segundo ea es Falso

simplificarEA'::EA->EA
simplificarEA' = foldEA Const simpBop

simpBop::BinOp -> EA -> EA -> EA
simpBop Sum e1 e2 = simpSumaEA e1 e2
simpBop Mul e1 e2 = simpMulEA e1 e2

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

evalEA'::EA->Int
evalEA' = foldEA id evalBOp

evalBOp::BinOp->Int->Int->Int
evalBOp Sum = (+)
evalBOp Mul = (*)

--showEA::EA->String

ea2ExpA'::EA->ExpA​
ea2ExpA' = foldEA Cte bop2ExpA

bop2ExpA::BinOp->ExpA->ExpA->ExpA
bop2ExpA Sum = Suma
bop2ExpA Mul = Prod

--ea2Arbol'::EA->AB Tree BinOp Int​

{-
Demostraciones

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

--Ej 3
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

foldT:: b -> (a -> b -> b -> b) -> Tree a -> b 
foldT fe fn EmptyT = fe
foldT fe fn (NodeT x ti td) = fn x (foldT fe fn ti) (foldT fe fn td) 

treeInt = NodeT 1 (NodeT 3 EmptyT EmptyT) (NodeT 4 (NodeT 5 EmptyT EmptyT) (EmptyT))
{-
       1
   3        4   
e    e    5   e
       e  e
-}

treeIntBST = NodeT 4 (NodeT 3 EmptyT EmptyT) (NodeT 6 (NodeT 5 EmptyT EmptyT) (EmptyT))
{-
       4
   3        6   
e    e    5   e
       e  e
-}

mapT :: (a -> b) -> Tree a -> Tree b
--mapT f = foldT EmptyT (\x rti rtd -> NodeT (f x) rti rtd)
mapT f = foldT EmptyT (NodeT .f)

sumT :: Tree Int -> Int
sumT = foldT 0 (\x ri rd -> x + ri + rd)

sizeT :: Tree a -> Int
sizeT = foldT 0 (\_ ri rd -> 1 + ri + rd)

heightT :: Tree a -> Int
heightT = foldT 0 (\_ ri rd -> 1 + ri `max` rd)

preOrder :: Tree a -> [a]
preOrder = foldT [] (\x ri rd -> x : ri ++ rd)

inOrder :: Tree a -> [a]
inOrder = foldT [] (\x ri rd -> ri ++ [x] ++ rd)

postOrder :: Tree a -> [a]
postOrder = foldT [] (\x ri rd -> ri ++ rd ++ [x])

mirrorT :: Tree a -> Tree a
--mirrorT = foldT EmptyT (\x ri rd -> NodeT x rd ri)
mirrorT = foldT EmptyT (\x -> flip (NodeT x) )

countByT :: (a -> Bool) -> Tree a -> Int
countByT f = foldT 0 (\x ri rd -> (if f x then 1 else 0) + rd+ ri)

partitionT :: (a -> Bool) -> Tree a -> ([a], [a])
--partitionT f = foldT ([],[]) (\x ri rd -> if f x then (x : (fst ri ++ fst rd), snd ri ++ snd rd) else (fst ri ++ fst rd, x : (snd ri ++ snd rd)))
partitionT f = foldT ([],[]) g 
  --where g x ri rd = if f x then (x : fst ri ++ fst rd, snd ri ++ snd rd) else (fst ri ++ fst rd, x : snd ri ++ snd rd)
  where g x (r1, r2) (r3, r4) = if f x then (x : r1++ r3, r2++r4) else (r1 ++ r3, x : r2 ++ r4)

zipWithT :: (a->b->c) -> Tree a -> (Tree b -> Tree c)
zipWithT f EmptyT EmptyT = EmptyT
zipWithT f EmptyT (NodeT x ri rd) = EmptyT
zipWithT f (NodeT x ri rd) EmptyT = EmptyT
zipWithT f (NodeT x rxi rxd) (NodeT y ryi ryd) = NodeT (f x y) (zipWithT f rxi ryi) (zipWithT f rxd ryd)

zipWithT' :: (a->b->c) -> Tree a -> Tree b -> Tree c
zipWithT' f t1 t2 = foldT (\_ -> EmptyT) g t1 t2
  where g x rxi rxd EmptyT = EmptyT
        g x rxi rxd (NodeT y ryi ryd) = NodeT (f x y) (rxi ryi) (rxd ryd)

caminoMasLargo :: Tree a -> [a]
caminoMasLargo = foldT [] (\x ri rd -> x : (if length ri >= length rd then ri else rd))

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x ri rd) = agregarAListasDeListas x (todosLosCaminos ri) (todosLosCaminos rd)

agregarAListasDeListas::a -> [[a]] -> [[a]] -> [[a]]
agregarAListasDeListas x xss yss = agregarAListas x xss ++ agregarAListas x yss

agregarAListas::a -> [[a]] -> [[a]]
agregarAListas x [] = [[x]]
agregarAListas x (xs:xss) = (x:xs):xss

--todosLosCaminos' :: Tree a -> [[a]]
--todosLosCaminos' = foldT [[]] (\x ri rd -> map (x:) (ri++rd) )

--todosLosNiveles :: Tree a -> [[a]]
-- ??

nivelN :: Tree a -> Int -> [a]
nivelN EmptyT n = []
nivelN (NodeT x ti td) 0 = [x]
nivelN (NodeT x ti td) n = nivelN ti (n-1) ++ nivelN td (n-1)

nivelN' :: Tree a -> Int -> [a]
nivelN' t n = foldT g z t n 
  where g n = []
        z x ti td 0 = [x]
        z x ti td n = ti (n-1) ++ td (n-1)

recT:: b -> (a -> Tree a -> b -> Tree a -> b -> b) -> Tree a -> b 
recT fe fn EmptyT = fe
recT fe fn (NodeT x ti td) = fn x ti (recT fe fn ti) td (recT fe fn td) 

insertT :: Ord a =>  a -> Tree a -> Tree a
insertT e EmptyT = NodeT e EmptyT EmptyT
insertT e (NodeT x ti td) = if e < x then NodeT x (insertT e ti) td else NodeT x ti (insertT e td)

insertT' :: Ord a =>  a -> Tree a -> Tree a
--insertT' e t = recT EmptyT (\x ti rti td rtd -> if e < x then NodeT x (NodeT e rti EmptyT) td else NodeT x ti (NodeT e rtd EmptyT) ) t 
insertT' e t = recT EmptyT g t 
  where g x ti rti td rtd = 
         if e < x 
             then NodeT x (NodeT e rti EmptyT) td 
             else NodeT x ti (NodeT e rtd EmptyT)

caminoHasta :: Eq a => a -> Tree a -> [a],
caminoHasta e EmptyT = [e]
caminoHasta e (NodeT x ti td) = (caminoHasta e ti) (caminoHasta e ti)

-- Ej 6

data Dir = Lt | Rt | Straight
data Mapa a = Cofre [a] | Nada (Mapa a) | Bifurcacion [a] (Mapa a) (Mapa a)

foldM::([a] -> b) -> (b -> b) -> ([a] -> b -> b -> b) -> Mapa a -> b
foldM fc fn fb (Cofre xs) = fc xs
foldM fc fn fb (Nada m) = fn (foldM fc fn fb m)
foldM fc fn fb (Bifurcacion xs m1 m2) = fb xs (foldM fc fn fb m1) (foldM fc fn fb m2)

recM::([a] -> b) -> (Mapa a -> b -> b) -> ([a] -> Mapa a -> b -> Mapa a -> b -> b)  -> Mapa a -> b
recM fc fn fb (Cofre xs) = fc xs
recM fc fn fb (Nada m) = fn m (recM fc fn fb m)
recM fc fn fb (Bifurcacion xs m1 m2) = fb xs m1 (recM fc fn fb m1) m2 (recM fc fn fb m2)

objects :: Mapa a -> [a]
objects = foldM id id (\xs rm1 rm2 -> xs ++ rm1 ++ rm2)

mapM :: (a -> b) -> Mapa a -> Mapa b
mapM f = foldM (\xs -> Cofre (map f xs)) Nada (\xs rm1 rm2 ->Bifurcacion (map f xs) rm1 rm2)

has :: (a -> Bool) -> Mapa a -> Bool
--has f = foldM (\xs ->any f xs) id (\xs rm1 rm2 ->((any f xs) || rm1) || rm2)
has f = foldM (any f) id (\xs rm1 rm2 ->(any f xs) || rm1 || rm2)

hasObjectAt :: (a->Bool) -> Mapa a -> [Dir] -> Bool 
hasObjectAt f = foldM c n b
  where c xs  []    = any f xs
        c xs (d:ds) = False -- error ""
        n m (Straight:ds) = m ds
        n m _ = False -- error "no hay camino disponible"
        b xs m1 m2 [] = any f xs
        b xs m1 m2 (Lt:ds) = m1 ds
        b xs m1 m2 (Rt:ds) = m2 ds
        b xs m1 m2 _ = False -- error "no hay camino disponible"

longestPath :: Mapa a -> [Dir]
longestPath = foldM (const []) (\r -> Straight:r) (\xs ri rd -> if length ri > length rd then Lt:ri else Rt:rd)

objectsOfLongestPath :: Mapa a -> [a]
objectsOfLongestPath
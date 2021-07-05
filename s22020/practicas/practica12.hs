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


cantDeSumaCeros = recExpA c s p
 where c n = 0
       s (Cte 0) e1 t2 e2 = 1 + e1 + e2
       s t1 e1 (Cte 0) e2 = 1 + e1 + e2
       s t1 e1 t2 e2 = e1 + e2
       p t1 e1 t2 e2 = e1 + e2

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

ea2ExpA'::EA->ExpA
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

caminoHasta :: Eq a => a -> Tree a -> [a]
caminoHasta e EmptyT = [e]
caminoHasta e (NodeT x ti td) = (caminoHasta e ti) ++ (caminoHasta e ti)

-- Ej 4 
--type Record a b = [(a,b)]
--select :: (Record a b -> Bool) -> [Record a b] -> [Record a b]


-- Ej 5


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
longestPath = foldM (const []) (\r -> Straight:r) (\xs ri rd -> if length ri >= length rd then Lt:ri else Rt:rd)

objectsOfLongestPath :: Mapa a -> [a]
objectsOfLongestPath = recM id (\m rm ->rm) (\xs m1 rm1 m2 rm2-> if longestMap m1 m2 then xs++rm1 else xs++rm2)

longestMap::Mapa a -> Mapa a -> Bool
longestMap m1 m2 = length (longestPath m1) >= length (longestPath m2) 

allPaths::Mapa a->[[Dir]]
allPaths = foldM (const [[]]) (addtToAll Straight) (\_ r1 r2 -> addtToAll Lt r1 ++ addtToAll Rt r2)

addtToAll :: Dir -> [[Dir]] -> [[Dir]]
addtToAll d []  = [[d]]
addtToAll d (xs:xss) = addAll d xs xss
--addtToAll dir = foldr (addAll dir) []

addAll :: Dir -> [Dir] -> [[Dir]] -> [[Dir]]
addAll d xs xss = [d:xs] ++ xss

objectsPerLevel::Mapa a->[[a]]
objectsPerLevel = foldM (\xs -> [xs]) (\rm ->[]:rm) (\xs rm1 rm2 -> xs: appendLevels rm1 rm2 )

appendLevels::[[a]]->[[a]]->[[a]]
appendLevels [] yss = yss
appendLevels xss [] = xss 
appendLevels (xs:xss) (ys:yss) =  (xs ++ ys) : appendLevels xss yss

mapa = Bifurcacion [1, 2, 3] (Cofre [4, 5, 6, 7]) (Cofre [7,7, 9])
mapa2 = Nada (Bifurcacion [1, 2, 3] (Cofre [10..20]) (Cofre [7,7, 9]))

-- Ej 7
data GTree a = GNode a [GTree a] deriving Show

foldGT0::(a -> [b] -> b) -> GTree a -> b
foldGT0 f (GNode e ts) = f e (map (foldGT0 f) ts) 

foldGT1 :: (a -> c -> b) -> (b -> c -> c) -> c -> GTree a -> b
foldGT1 g f z (GNode e xs) = g e (foldr f z (map (foldGT1 g f z) xs))

foldGT :: (a -> c -> b) -> ([b] -> c) -> GTree a -> b
foldGT g f (GNode e xs) = g e (f (map (foldGT g f) xs))

recGT0 :: (a -> [GTree a] -> [b] -> b) -> GTree a -> b
recGT0 f (GNode x xs) = f x xs (map (recGT0 f) xs)

--recGT1 :: (a -> c -> [GTree a] -> b) -> ([GTree a] -> b -> c -> c) -> c -> GTree a -> b
--recGT1 g f z (GNode e xs) = g e xs (recr f z (map (recGT1 g f z) xs))

recGT :: (a -> c -> [GTree a] -> b) -> ([b] -> c) -> GTree a -> b
recGT g f (GNode e xs) = g e (f (map (recGT g f) xs)) xs

mapGT :: (a -> b) -> GTree a -> GTree b
--mapGT f (GNode e ts) = GNode (f e) (map (mapGT f) ts )
--mapGT f = foldGT0 (\e ts -> GNode (f e) ts)
--mapGT f = foldGT1 (\e ts -> GNode (f e) ts) (:) []
mapGT f = foldGT (\e n -> GNode (f e) n) id

sumGT :: GTree Int -> Int
--sumGT (GNode e ts) = e + sum (map sumGT ts)
--sumGT = foldGT0 (\x ts -> x + sum ts)
--sumGT = foldGT1 (\x ts -> x + ts) (\x xs-> x + xs) 0
{- (\x ts -> x + ts) -> x es el e de GNode e ts, ts es la recursiòn procesada sobre la listas una vez que se realizó
(\x rxs-> x + rxs) -> x es el elemento actual sobre la lista y rxs es la parte recusiva procesada sin xs (vease como x: recxs)
0 es el caso base del foldr para la función anterior. -}
sumGT = foldGT (+) sum

depthGT = foldGT (\x d -> 1+d) (maxOr 0)
 where maxOr x [] = x
       maxOr _ xs = maximum xs

sizeGT :: GTree a -> Int
--sizeGT (GNode e ts) = 1 + sum (map sizeGT ts)
--sizeGT = foldGT0 (\_ ts -> 1 + sum ts)
sizeGT = foldGT1 (\x ts -> 1 + ts) (\x xs-> x + xs) 0
--sizeGT = foldGT (\x c -> 1+c) sum

heightGT :: GTree a -> Int
--heightGT (GNode e ts) = 1 + maxOr 0 (map heightGT ts)
--heightGT = foldGT0 (\_ ts -> 1 + maxOr 0 ts)
--heightGT = foldGT1 (\_ ts -> ts) (\x xs-> 1 + xs) 0
heightGT = foldGT (\x c -> 1 + c) (maxOr 0)

maxOr::Ord a=> a-> [a]-> a
maxOr x [] = x
maxOr _ xs = maximum xs

preOrderGT :: GTree a -> [a]
--preOrderGT (GNode e ts) = e : concat (map preOrderGT ts)
--preOrderGT = foldGT0 (\e ts -> e : concat ts)
--preOrderGT = foldGT1 (\x ts -> x: concat ts) (\x xs -> x: xs) [] -- foldGT1 (\x ts -> x: concat ts) (:) [] 
preOrderGT = foldGT (\x c-> x : c) concat

postOrderGT :: GTree a -> [a]
--postOrderGT (GNode e ts) = concat (map postOrderGT ts) ++ [e]
--postOrderGT = foldGT0 (\e ts -> concat ts++[e])
--postOrderGT = foldGT1 (\e ts -> concat ts++[e]) (\x xs -> x:xs) []  -- foldGT1 (\e ts -> concat ts++[e]) (:) [] 
postOrderGT = foldGT (\x c-> c++[x]) concat

mirrorGT :: GTree a -> GTree a
--mirrorGT (GNode e ts) = GNode e  (reverse (map mirrorGT ts))
--mirrorGT  = foldGT0 (\e ts ->GNode e (reverse ts))
--mirrorGT  = foldGT1 (\e ts ->GNode e ts) (\x xs -> xs++[x]) []
mirrorGT  = foldGT GNode reverse --foldGT (\e c ->GNode e c) reverse

countByGT :: (a -> Bool) -> GTree a -> Int
--countByGT f (GNode e ts) = unoSiCumple f e + sum (map (countByGT f) ts)
--countByGT f  = foldGT0 (\e ts ->unoSiCumple f e + sum ts)
--countByGT f  = foldGT1 (\e ts ->unoSiCumple f e + ts) (+) 0
countByGT f  = foldGT (\e c ->unoSiCumple f e + c) sum

unoSiCumple:: (a -> Bool) -> a -> Int
unoSiCumple f e = if f e then 1 else 0

partitionGT :: (a -> Bool) -> GTree a -> ([a], [a])
--partitionGT p (GNode e ts) = f e (aplanarPares (map (partitionGT p) ts))
-- where f x (r1,r2) = if p x then (x:r1,r2) else (r1,x:r2) 
partitionGT p = foldGT f g                                        
 where f x (r1,r2) = if p x then (x:r1,r2) else (r1,x:r2) 
       g r1 = aplanarPares r1

aplanarPares:: [([a],[b])] -> ([a],[b])
aplanarPares [] = ([],[])
aplanarPares ((x,y):xs) = (x ++ fst (aplanarPares xs),y ++ snd (aplanarPares xs))


{-
caminoMasLargoGT = foldGT b (maxOr 0)
 where b e [] = [e]
       b e (x:xs) = e:(x:xs)
-}

--caminoMasLargoGT = foldGT0 (\x ts ->x : maximum ts ) 
--caminoMasLargoGT = foldGT0 (\x ts ->x : concat (ts) ) 

--todosLosCaminosGT :: GTree a -> [[a]]
--todosLosCaminosGT = foldGT1 (\x ts -> concat (map ([x]:) ts))
--todosLosCaminosGT (GNode a ts)= (map (a:) (concat (map todosLosCaminosGT ts)))

caminoHastaGT :: Eq a => a -> GTree a -> [a]
caminoHastaGT a = foldGT (:) g
  where g xs = findList a xs 

findList a [] = []
findList a (x:xs) = if any (==a) x 
                      then x 
                      else findList a xs

gtree = GNode 1 [GNode 2 [GNode 5 [], GNode 6 []
                ] , 
        GNode 3 [GNode 7 []], GNode 4 []
        ]

{-
1 
    2
       5
       6
    3
       7
    4
-}


{-type Name = String
type Content = String
type Path = [Name]
data FileSystem = File Name Content | Folder Name [FileSystem] deriving Show

foldFS :: (Name -> Content -> b) -> (Name -> c -> b) -> ( [b] -> c) -> FileSystem -> b
foldFS f g h (File n c ) = f n c
foldFS f g h (Folder n fs) =  g n (h (map (foldFS f g h) fs)) 

recFS :: (Name -> Content -> b) -> (Name -> c -> b) -> ( [FileSystem] -> [b] -> c) -> FileSystem -> b
recFS f g h (File n c ) = f n c
recFS f g h (Folder n fs) =  g n (h fs (map (recFS f g h) fs)) 


amountOfFiles :: FileSystem -> Int
amountOfFiles = foldFS (\n c -> 1) (\n rfs -> 1+ rfs) sum

find :: Name -> FileSystem -> Maybe Content
find n = foldFS (\n' c -> if n' == n then Just c else Nothing) (\n' rfs -> aplanarMaybe rfs) (\rfs -> rfs)

aplanarMaybe::[Maybe a] -> Maybe a
aplanarMaybe [Nothing] = Nothing
aplanarMaybe [Just c] = Just c
aplanarMaybe (m:ms) = if isNothing m then Nothing else aplanarMaybe ms

isNothing::Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

pathOf :: Name -> FileSystem -> Path
--pathOf n = foldFS (\n' c -> if n == n' then [n] else []) (\n' rfs -> n':rfs) concat
pathOf n = foldFS (\n' c -> if n == n' then [n] else []) (:) concat

mapContents :: (Content -> Content) -> FileSystem -> FileSystem
--mapContents f (File n c ) = File n (f c)
--mapContents f (Folder n fs) = Folder n (map (mapContents f) fs)
mapContents f = foldFS (\n c -> File n (f c) ) (\n rfs ->Folder n rfs) id

-}

type Name = String
type Content = String
type Path = [Name]
data FileSystem = File Name Content | Folder Name [FileSystem] deriving Show

fs = Folder "Juegos" [File "WOW" "wow1", File "Cod" "cod2", Folder "Nuevos" [File "Cyberpunk" "2020cp"] ]

foldFS::(Name -> Content -> b)-> (Name -> c -> b) -> ([b] -> c)  -> FileSystem -> b
foldFS fn ff fg (File n c) = fn n c
foldFS fn ff fg (Folder n fss) = ff n (fg (map (foldFS fn ff fg) fss))

foldFS0::(Name -> Content -> b)-> (Name -> [b] -> b)  -> FileSystem -> b
foldFS0 fn ff (File n c) = fn n c
foldFS0 fn ff (Folder n fss) = ff n (map (foldFS0 fn ff ) fss)

foldFS1::(Name -> Content -> b) -> (Name -> c -> b) -> (b -> c -> c) -> c -> FileSystem -> b
foldFS1 fn ff fe fx (File n c) = fn n c
foldFS1 fn ff fe fx (Folder n fss) = ff n (foldr fe fx (map (foldFS1 fn ff fe fx) fss))


amountOfFiles :: FileSystem -> Int
amountOfFiles = foldFS (\n c -> 1) (\n c -> c) sum

find :: Name -> FileSystem -> Maybe Content
--find nom = foldFS0 (\nom' cont -> if nom == nom' then Just cont else Nothing) (\nom' rs -> firstMaybe rs)
find nom = foldFS (\nom' cont -> if nom == nom' then Just cont else Nothing) (\nom' rs -> firstMaybe rs) id

firstMaybe :: [Maybe a] -> Maybe a
firstMaybe = foldr (\x r -> case x of
                              Nothing -> r
                              Just _  -> x) Nothing
							  
pathOf :: Name -> FileSystem -> Path
pathOf nm = foldFS (\n c -> if n== nm then [n] else []) (\n rc -> n : concat rc ) id

--- EXTRA 

data ExpG = Constante Int | ApplyOp OpG [ExpG] deriving Show
data OpG = Sumatoria | Productoria | Promedio deriving Show

foldEG::(Int -> b) -> (OpG -> c -> b) -> ([b] -> c) -> ExpG -> b
foldEG fc fo fe (Constante n) = fc n
foldEG fc fo fe (ApplyOp op es) = fo op (fe (map (foldEG fc fo fe) es))

recEG::(Int -> b) -> (OpG -> [ExpG] -> c -> b) -> ([b] -> c) -> ExpG -> b
recEG fc fo fe (Constante n) = fc n
recEG fc fo fe (ApplyOp op es) = fo op es (fe (map (recEG fc fo fe) es))

-- Reglas
-- 1) Sin listas vacias
-- 2) Sin sumatorias de cero (listas solo con ceros)
-- 3) Sin productorias de uno (listas solo con unos)

evalG :: ExpG -> Int
evalG = foldEG (\n -> n) (\op is -> appOpALista op is) id

appOpALista::OpG-> [Int]-> Int
appOpALista Sumatoria xs = foldr (+) 0 xs
appOpALista Promedio xs = div (foldr (+) 1 xs) (length xs)
appOpALista Productoria xs =  foldr (*) 0 xs

simpG :: ExpG -> ExpG
simpG (Constante n) = Constante n
simpG (ApplyOp op exs) = simpOpG op (map simpG exs)

simpOpG:: OpG -> [ExpG] -> ExpG
simpOpG Sumatoria ex = ApplyOp Sumatoria (filter esCero ex)
simpOpG Productoria ex = ApplyOp Productoria (filter esUno ex)
simpOpG Promedio ex = ApplyOp Promedio ex

esCero:: ExpG -> Bool
esCero (Constante 0) = True
esCero  _            = False

esUno:: ExpG -> Bool
esUno (Constante 1) = True
esUno  _            = False


data NExp = Var Variable | NCte Int | NBOp NBinOp NExp NExp 
data NBinOp = Add | Sub | Mul' | Div | Mod | Pow 
type Variable = String

foldNExp :: (NBinOp -> b -> b -> b) -> (Int -> b) -> (Variable -> b) -> NExp -> b
foldNExp f g h (Var variable)           = h variable
foldNExp f g h (NCte int)               = g int 
foldNExp f g h (NBOp binOp nExp1 nExp2) = f binOp (foldNExp f g h nExp1) (foldNExp f g h nExp2)

recNExp :: (NBinOp -> NExp -> NExp -> b -> b -> b) -> (Int -> b) -> (Variable -> b) -> NExp -> b
recNExp f g h = appDup (foldNExp f' g' h')
 where h' var _ = h var
       g' int _ = g int
       f' op r1 r2 (NBOp op' nExp1 nExp2) = f op' nExp1 nExp2 (r1 nExp1) (r2 nExp2)

appDup f x = f x x


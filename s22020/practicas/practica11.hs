data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show
data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamon | Queso | Salsa deriving Show

-- Ej 1
cantidadCapasQueCumplen::(Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen f Prepizza = 0
cantidadCapasQueCumplen f (Capa i p) = if f i then 1 else 0 + cantidadCapasQueCumplen f p

conCapasTransformadas::(Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas f Prepizza = Prepizza
conCapasTransformadas f (Capa i p) = Capa (f i) (conCapasTransformadas f p)

soloLasCapasQue:: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue f Prepizza = Prepizza
--soloLasCapasQue f (Capa i p) = if f i then Capa i (soloLasCapasQue f p) else soloLasCapasQue f p
soloLasCapasQue f (Capa i p) = (if f i then Capa i else id) (soloLasCapasQue f p)

-- Ej 2

sinLactosa :: Pizza -> Pizza
sinLactosa p = soloLasCapasQue (not . esQueso) p

esQueso::Ingrediente-> Bool
esQueso Queso = True
esQueso _	  = False

aptaIntolerantesLactosa:: Pizza -> Bool
--aptaIntolerantesLactosa = 0 == (cantidadCapasQueCumplen esQueso)
aptaIntolerantesLactosa = (==0) . (cantidadCapasQueCumplen esQueso)

cantidadDeQueso :: Pizza -> Int
cantidadDeQueso = cantidadCapasQueCumplen esQueso

conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas = conCapasTransformadas dobleAcc

dobleAcc::Ingrediente->Ingrediente
dobleAcc (Aceitunas n) = Aceitunas (2*n)
dobleAcc i = i 

-- Ej 3
pizzaProcesada::(Ingrediente->b->b) -> b -> Pizza -> b
pizzaProcesada fc fp Prepizza = fp
pizzaProcesada fc fp (Capa i p) = fc i (pizzaProcesada fc fp p)

-- Ej 4
cantidadCapasQueCumplen' :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen' f = pizzaProcesada (\i rp -> (if f i then 1 else 0) + rp) 0

conCapasTransformadas'::(Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas' f = pizzaProcesada (\i rp -> Capa (f i) rp) Prepizza

soloLasCapasQue':: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue' f = pizzaProcesada (\i rp -> (if (f i ) then Capa i else id) rp) Prepizza

sinLactosa':: Pizza -> Pizza
--sinLactosa' = pizzaProcesada (\i rp-> if (not . esQueso) i then Capa i rp else rp ) Prepizza
sinLactosa' = pizzaProcesada (\i rp-> (if (not . esQueso) i then Capa i else id) rp ) Prepizza

aptaIntolerantesLactosa':: Pizza -> Bool
aptaIntolerantesLactosa' = pizzaProcesada (\i rp -> if (not . esQueso) i then rp else False) True 

cantidadDeQueso' :: Pizza -> Int
cantidadDeQueso' = pizzaProcesada (\i rp -> (if esQueso i then 1 else 0) + rp) 0

conElDobleDeAceitunas' :: Pizza -> Pizza
conElDobleDeAceitunas' = pizzaProcesada (\i rp -> Capa (dobleAcc i) rp) Prepizza

-- Ej 5
cantidadAceitunas :: Pizza -> Int
cantidadAceitunas = pizzaProcesada (\i rp -> aceitunas i + rp ) 0

aceitunas::Ingrediente->Int
aceitunas (Aceitunas n) = n
aceitunas _ = 0

capasQueCumplen  :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen f = pizzaProcesada (\i rp -> if f i then i : rp else rp) []

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada = pizzaProcesada (\i rp ->descMejorada i rp) Prepizza

descMejorada::Ingrediente -> Pizza -> Pizza
descMejorada (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n + m)) p
descMejorada i p = Capa i p

conCapasDe::Pizza->Pizza->Pizza 
--conCapasDe Prepizza   p2 = p2
--conCapasDe (Capa i p) p2 = Capa i (conCapasDe p p2)
conCapasDe p = flip (pizzaProcesada(\i rp -> Capa i rp)) p

cantidadDe:: (Ingrediente->Bool) -> Pizza -> Int
cantidadDe f = pizzaProcesada (\i rp -> (if f i then 1 else 0) + rp ) 0

-- Ej 6
{-
Demostraciones


-}

-- Ej 7

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs) = (if f x then (x:) else id) (filter f xs)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f g [] = g
foldr f g (x:xs) = f x (foldr f g xs) 

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr f g []     = f
recr f g (x:xs) = g xs (recr f g xs)

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f [] = error ("no hay elementos")
foldr1 f [x] = x
foldr1 f (x:xs) = f x (foldr1 f xs)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] ys = ys
zipWith f xs [] = xs
zipWith f (x:xs) (y:ys) = f x y : zipWith xs ys


-- Ej 8 
{- 
Demostraciones sobre listas

-}

-- Ej 9

sum' :: [Int] -> Int
--sum' = foldr (\x rx -> x + rx) 0
sum' = foldr (+) 0

length' :: [a] -> Int
length' = foldr (\x rx -> 1 + rx) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x rx -> f x : rx) []

filter :: (a -> Bool) -> [a] -> [a]
--filter f = foldr (\x rx -> if f x then x : rx else rx) []
filter f = foldr (\x rx -> (if f x then (x:) else id) rx) []

find :: (a -> Bool) -> [a] -> Maybe a
find f = foldr (\x rx -> if f x then Just x else rx) Nothing

any' :: (a -> Bool) -> [a] -> Bool
--any' f = foldr (\x rx -> if f x then True || rx else rx) False 
--any' f = foldr (\x rx -> f x || rx ) False 
any' f = foldr ( (||) . f) False 

all'' :: (a -> Bool) -> [a] -> Bool
all' f = foldr ( (&&) . f) True 

countBy :: (a -> Bool) -> [a] -> Int
countBy f = foldr (\x rx -> (if f x then 1 else 0) + rx) 0

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f = foldr g ([],[]) 
  where g x r = 
          if f x 
            then (x:fst r, snd r) 
            else (fst r, x: snd r)
			

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith' f xs ys = recr 
--         (\x xs r ys -> 
--           case ys of
--                [] -> (x:xs)
--                (y:ys) = f x y (r ys)
--         id
--         xs 
--         ys

zipWith' f xs ys = recr g id xs ys
    -- where g x xs r ys =
    -- 	    case ys of
    --            [] -> (x:xs)
    --            (y:ys) = f x y (r ys)
    where g x xs r [] = (x:xs)
    	  g x xs r (y:ys) = f x y (r ys)


takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f [] = []
takeWhile f (x:xs) = if f x then x: takeWhile f xs else takeWhile f xs

--take [] n     = []
--take (x:xs) n = x : take xs (n-1)  
		
take' :: [a] -> Int -> [a]
take' = foldr' g z
  where z n  = []
        g x r 0 = []
        g x r n = x : r (n-1) -- en este caso r es la recursión take xs
		

drop :: [a] -> Int -> [a]
--drop = foldr g [] 
--  where g x r 0 = x: ??    <- como acá me falta xs uso recr
--		g x r n = 
drop = recr g z
  where z n = []
        g x xs r 0 = x:xs
        g x xs r n = r (n-1)-- en este caso r es la recursión drop xs
          
		  
takeWhile' :: (a -> Bool) -> [a] -> [a]
--takeWhile' f [] = []
--takeWhile' f (x:xs) = if f x then x: takeWhile' f xs else []
takeWhile' f = foldr g []
  where g x r =
          if f x 
            then x : r
            else []
			
(!!!) :: [a] -> Int -> Maybe a
--(!!!) [] n = Nothing
--(!!!) (x:xs) 0 = Just x
--(!!!) (x:xs) n = (!!!) xs (n-1)
(!!!) = foldr g z
  where z n = Nothing
        g x r 0 = Just x
        g x r n' = r (n'-1)

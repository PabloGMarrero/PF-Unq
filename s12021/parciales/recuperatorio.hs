data Nota = Do | Re | Mi | Fa | Sol | La | Si deriving Show
type Tiempo = Int -- el instante en el que suena una nota
type Duracion = Int -- cantidad de tiempos que suena una nota
data Comp = Silencio Duracion | Pulso Nota Duracion | Arpegio Comp Comp | Acorde Comp Comp deriving Show

type Midi = [[Nota]]
-- notas simultáneas para cada tiempo en la duración de la melodía
type Sampler = Tiempo -> [Nota]
-- función que da las notas que suenan en un tiempo dado

compEj = Arpegio (Pulso Do 2) (Acorde (Pulso Mi 3) (Pulso Sol 4))
--Ej 1) Definir las funciones duracion :: Comp -> Int, que describe la duración total de la composición dada, y alargar :: Comp -> Int -> Int, que dada una composición, describe la composición que resulta de alargar cada nota o silencio por el factor dado.

duracion :: Comp -> Int
duracion (Silencio durN) = durN
duracion (Pulso no durN)= durN
duracion (Arpegio c1 c2) = (+) (duracion c1) (duracion c2)
duracion (Acorde c1 c2) = (+) (duracion c1) (duracion c2)

alargar :: Comp -> Int -> Comp
alargar (Silencio durN) n = Silencio (n * durN)
alargar (Pulso no durN) n = Pulso no (durN * n)
alargar (Arpegio c1 c2) n = Arpegio (alargar c1 n) (alargar c2 n)
alargar (Acorde c1 c2)  n = Acorde (alargar c1 n) (alargar c2 n)

-- Ej 2) Definir la función sintetizar :: Comp -> Midi, que describe el Midi equivalente a la composición dada.
sintetizar :: Comp -> Midi
sintetizar (Silencio n) = []
sintetizar (Pulso n d) = [[n]]
sintetizar (Arpegio e1 e2) = (sintetizar e1) ++ (sintetizar e2)
sintetizar (Acorde e1 e2) = concatPerLevel (sintetizar e1) (sintetizar e2)

concatPerLevel :: [[a]] -> [[a]] -> [[a]]
concatPerLevel [] []        = []
concatPerLevel [] ys         = ys
concatPerLevel xs []         = xs
concatPerLevel (x:xs) (y:ys) = (x ++ y) : concatPerLevel xs ys

-- Ej 3) Demostrar que para todo  k :: Int, para todo c :: Comp. k>=0 entonces duracion (alargar k c) = k * duracion c.

{-

	duracion (alargar c k) = k * duracion c?

DEM: Sea c un Comp. Por principio de inducción en la estructura de c.

HIP dada: ¡ k>=0 !


- Caso base 1: c= Silencio n
	duracion (alargar (Silencio n) k) = k * duracion (Silencio n)?

- Caso base 2: c= Pulso n d
	duracion (alargar (Pulso n d) k) = k * duracion (Pulso n d)?

- Caso ind 1: c= (Arpegio e1 e2)
	TI:  duracion (alargar (Arpegio e1 e2) k) = k * duracion (Arpegio e1 e2)?
	HI1: duracion (alargar e1 k) = k * duracion e1!
	HI1: duracion (alargar e2 k) = k * duracion e2!
	
- Caso ind 1: c= (Acorde e1 e2)
	TI:  duracion (alargar (Acorde e1 e2) k) = k * duracion (Acorde e1 e2)?
	HI1: duracion (alargar e1 k) = k * duracion e1!
	HI1: duracion (alargar e2 k) = k * duracion e2!
	
Demostración

- Caso base 1
	LADO IZQ
	duracion (alargar (Silencio n) k ) 
	= alargar.1 
	duracion (Silencio (n * k))
	= duracion.1
	n * k
	
	LADO DER
	k * duracion (Silencio n)
	= duracion.1
	k * n
	= aritmetica
	n * k
	
	LLEGUÉ
	
- Caso base 2
	LADO IZQ
	duracion (alargar k (Pulso n d)) 
	= alargar.2
	duracion (Pulso no (d * k))
	= duracion.2
	d * k
	
	LADO DER
	k * duracion (Pulso n d)
	= duracion.2
	k * d
	= aritmetica
	d * k
	
	LLEGUÉ
	
- Caso ind 1
	LADO IZQ
	duracion (alargar (Arpegio e1 e2) k ) 
	= alargar.3
	duracion (Arpegio (alargar e1 k) (alargar e2 k))
	= duracion.3
	(+) (duracion (alargar e1 k)) (duracion (alargar e2 k))
	= HI1
	(+) (k * duracion e1) (duracion (alargar e2 k))
	= HI2
	(+) (k * duracion e1) (k * duracion e2)
	= factor comun e HIP dada
	k * ( (+) (duracion e1) (duracion e2))
	
	LADO DER
	k * duracion (Arpegio e1 e2)
	= duracion.3
	k * ( (+) (duracion e1) (duracion e2))
	
	LLEGUÉ
	
- Caso ind 2
	LADO IZQ
	duracion (alargar (Acorde e1 e2) k) 
	= alargar.4
	duracion (Acorde (alargar e1 k) (alargar e2 k))
	= duracion
	(+) (duracion (alargar e1 k)) (duracion (alargar e2 k))
	= HI1
	(+) (k * duracion e1) (duracion (alargar e2 k))
	= HI2
	(+) (k * duracion e1) (k * duracion e2)
	= factor comun e HIP dada
	k * ((+) (duracion e1) (duracion e2))
	= duracion.4
	k * duracion (Acorde e1 e2)
	
	LLEGUÉ
	
-}

--Ej 4) Indicar el tipo y dar una implementación de una función que exprese la recursión primitiva sobre las composiciones.

recCm:: (Duracion -> b) -> (Nota -> Duracion -> b) -> (Comp -> b -> Comp -> b -> b ) -> (Comp -> b -> Comp -> b -> b ) -> Comp -> b
recCm s p a ac (Silencio d)    = s d
recCm s p a ac (Pulso nota d)  = p nota d
recCm s p a ac (Arpegio c1 c2) = a c1 (recCm s p a ac c1) c2 (recCm s p a ac c2)
recCm s p a ac (Acorde c1 c2)  = ac c1 (recCm s p a ac c1) c2 (recCm s p a ac c2)

-- Ej 5) Indicar el tipo y dar una implementación de una función que exprese la recursión estructural sobre las composiciones sin utilizar recursión explícita.

foldCm ::(Duracion -> b) -> (Nota -> Duracion -> b) -> (b -> b -> b) -> (b -> b -> b) -> Comp -> b
foldCm s p a ac = recCm s p (\c1 rc1 c2 rc2-> a rc1 rc2) (\c1 rc1 c2 rc2-> ac rc1 rc2)

-- Ej 6) Definir, expresadas como recursión estructural implícita, las funciones del ejercicio 1 (duracion :: Comp -> Int, que describe la duración total de la composición dada, y alargar :: Comp -> Int -> Comp, que dada una composición, describe la composición que resulta de alargar cada nota o silencio por el factor dado.)

duracion' :: Comp -> Int
duracion' = foldCm id (\n d -> d) (+) (+)

alargar' :: Comp -> Int -> Comp
alargar' c = foldCm g h j k c 
 where g d       = (\n -> Silencio (d * n))
       h no d    = (\n -> Pulso no (d * n))
       j rc1 rc2 = (\n -> Arpegio (rc1 n) (rc2 n))
       k rc1 rc2 = (\n -> Acorde (rc1 n) (rc2 n))

-- Ej 7) Definir, expresada como recursión estructural implícita, la función del ejercicio 2 (sintetizar :: Comp -> Midi, que describe el Midi equivalente a la composición dada).

sintetizar' :: Comp -> Midi
sintetizar' = foldCm g h i j 
 where g d       = []
       h no d    = [[no]]
       i rc1 rc2 = rc1 ++ rc2
       j rc1 rc2 = concatPerLevel rc1 rc2
	   
-- Ej 8) Definir, sin utilizar recursión estructural explícita, la función unir :: [Comp] -> [Comp] -> Comp, que dadas dos listas de composiciones de igual longitud, describe una composición donde las partes de la misma son las composiciones de cada posición de las listas dadas sonando en simultáneo. Así, las dos primeras composiciones suenan juntas, y al terminar suenan juntas las dos segundas, luego las dos terceras juntas, etc.

unir :: [Comp] -> [Comp] -> Comp
--unir [] [] = Silencio 0
--unir (x:xs) (y:ys) = Arpegio (Acorde x y) (unir xs ys)
unir xs ys = foldr g (Silencio 0) xs
 where g x r = (\y -> Arpegio (Acorde x y)) r (head ys)
 
--Arpegio (Acorde (Arpegio (Pulso Do 2) (Acorde (Pulso Mi 3) (Pulso Sol 4))) (Silencio 0)) (Arpegio (Pulso Do 2) (Acorde (Pulso Mi 3) (Pulso Sol 4)))


--rec explicita
--Arpegio (Acorde (Arpegio (Pulso Do 2) (Acorde (Pulso Mi 3) (Pulso Sol 4))) (Arpegio (Pulso Do 2) (Acorde (Pulso Mi 3) (Pulso Sol 4)))) (Silencio 0)
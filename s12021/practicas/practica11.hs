-- Ejercicio 1) Definir las siguientes funciones utilizando recursión estructural explícita sobre Pizza:
data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show
data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamon | Queso | Salsa deriving Show

cantidadCapasQueCumplen:: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen f (Prepizza) = 0
cantidadCapasQueCumplen f (Capa i p) =  if f i then 1 else 0 + cantidadCapasQueCumplen f p

conCapasTransformadas:: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas f (Prepizza) = Prepizza
conCapasTransformadas f (Capa i p) =  Pizza (f i) conCapasTransformadas f p

soloLasCapasQue:: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue f (Prepizza) = Prepizza
soloLasCapasQue f (Capa i p) =  (if f i then Capa i else id) (soloLasCapasQue f p)

-- Ejercicio 2) Definir las siguientes funciones utilizando alguna de las definiciones anteriores:

sinLactosa :: Pizza -> Pizza
sinLactosa p = soloLasCapasQue (not . esQueso) p

esQueso::Ingrediente-> Bool
esQueso Queso = True
esQueso i     = False

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa p = cantidadCapasQueCumplen esQueso p == 0

cantidadDeQueso :: Pizza -> Int
cantidadDeQueso p = cantidadCapasQueCumplen esQueso p
	
conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas p = conCapasTransformadas dobleAceitunas p

dobleAceitunas:: Ingrediente -> Ingrediente
dobleAceitunas (Aceitunas n) = Aceitunas (n*2)
dobleAceitunas i             = i

--Ejercicio 3) Definir pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b que expresa la definición de fold para la estructura de Pizza.
pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
pizzaProcesada f g (Prepizza)  = g
pizzaProcesada f g (Pizza i p) = f i (pizzaProcesada f g p)

--Ejercicio 4) Resolver todas las funciones de los puntos 1) y 2) utilizando la función pizzaProcesada
cantidadCapasQueCumplen f = pizzaProcesada (\i rp -> if f i then 1 else 0 + rp) 0

conCapasTransformadas f = pizzaProcesada (\i rp -> Pizza (f i) rp) Prepizza

soloLasCapasQue f = pizzaProcesada (\i rp -> (if f i then Capa i else id) rp ) Prepizza

sinLactosa = pizzaProcesada (\i rp-> (if (not . esQueso) i then Capa i else id) rp ) Prepizza

aptaIntolerantesLactosa = pizzaProcesada (\i rp -> if (not . esQueso) i then rp else False) True

conElDobleDeAceitunas = pizzaProcesada (\i rp -> Capa (dobleAceitunas i) rp) Prepizza


--Ejercicio 5) Resolver las siguientes funciones utilizando pizzaProcesada (si resulta demasiado complejo resolverlas, dar primero una definición por recursión estructural explícita, y usar la técnica de los “recuadros”):

cantidadAceitunas :: Pizza -> Int
cantidadAceitunas = pizzaProcesada (\i rp -> if aceitunas i then 1 else 0 + rp ) 0

aceitunas (Aceitunas n) = n
aceitunas i             = i 

capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen f = pizzaProcesada (\i rp -> if f i then i:rp else rp ) []

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada = pizzaProcesada (\i rp -> descMejorada i rp) Prepizza

descMejorada::Ingrediente -> Pizza -> Pizza
descMejorada (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n + m)) p
descMejorada i p = Capa i p

conCapasDe :: Pizza -> Pizza -> Pizza -- que agrega las capas de la primera pizza sobre la segunda
--conCapasDe Prepizza   p2 = p2
--conCapasDe (Capa i p) p2 = Capa i (conCapasDe p p2)
conCapasDe p = flip (pizzaProcesada(\i rp -> Capa i rp)) p

primerasNCapas :: Int -> Pizza -> Pizza
--primerasNCapas 0 Prepizza = Prepizza
--primerasNCapas n (Capa i p) = Capa i (primerasNCapas (n-1) p)
primerasNCapas = flip (pizzaProcesada ctk (\n-> Prepizza))
 where ctk i h = \n-> if n==0 then Prepizza else Capa i (h (n-1))
 
--Ejercicio 6) Demostrar las siguientes propiedades sobre el tipo Pizza, utilizando las definiciones por recursión estructural explícita para cada función:

{-
a. para todo f. length . capasQueCumplen f = cantidadDe f

Por ppio de extensionalidad sea p una Pizza cualquiera
	(length . capasQueCumplen f) p = cantidadDe f p
Por def (.)
	length (capasQueCumplen f p )= cantidadDe f p
	
Por inducción en la estructura de p
- Caso base p = Prepizza
	length (capasQueCumplen f Prepizza )= cantidadDe f Prepizza
- Caso ind p  = Capa i p'
	TI: length (capasQueCumplen f (Capa i p') ) = cantidadDe f (Capa i p')?
	HI: length (capasQueCumplen f p')= cantidadDe f p'!
	
Demostración 
- Caso base
	length (capasQueCumplen f Prepizza )
	= def capasQueCumplen
	length (pizzaProcesada (\i rp -> if f i then i:rp else rp ) [] Prepizza)
	= pizzaProcesada.1
	length []
	= length.1
	0
	= cantidadDe.1
	cantidadDe f Prepizza
	
- Caso ind
	length (capasQueCumplen f (Capa i p') )
	= def capasQueCumplen
	length (pizzaProcesada (\i rp -> if f i then i:rp else rp ) [] Capa i p'))
	= pizzaProcesada.2
	length ((\i rp -> if f i then i:rp else rp ) i (pizzaProcesada (\i rp -> if f i then i:rp else rp ) [] p')) 
	= def capasQueCumplen
	length ((\i rp -> if f i then i:rp else rp ) i (capasQueCumplen f p'))
	= beta reducción
	length (if f i then i:(capasQueCumplen f p') else (capasQueCumplen f p') ) 
		
	cantidadDe f (Capa i p')
	= cantidadDe.2
	cantidadCapasQueCumplen f (Capa i p')
	= cantidadCapasQueCumplen.2
	if f i then 1 else 0 + cantidadCapasQueCumplen f p
	
	
	Caso f i =  True
		length (if True then i:(capasQueCumplen f p') else (capasQueCumplen f p') )
		=
		length (i:(capasQueCumplen f p'))
		= length.1
		1 + length (capasQueCumplen f p')
		= HI
		1 + cantidadDe f p'
		= cantidadDe.2
		1 + cantidadCapasQueCumplen f p'
		
		
		if True then 1 else 0 + cantidadCapasQueCumplen f p
		= def if
		1 + cantidadCapasQueCumplen f p
		
		LLEGUÉ
		
	Caso f i = False
		length (if False then i:(capasQueCumplen f p') else (capasQueCumplen f p') )
		= def if 
		length (capasQueCumplen f p')
		= HI
		cantidadDe f p'
		= cantidadDe.2
		cantidadCapasQueCumplen f p'
		
		if False then 1 else 0 + cantidadCapasQueCumplen f p
		= def if
		0 + cantidadCapasQueCumplen f p
		= aritmetica
		cantidadCapasQueCumplen f p
		
		LLEGUÉ


b. para todo f. para todo p1. para todo p2.
	cantidadCapasQueCumplen f (conCapasDe p1 p2) = cantidadCapasQueCumplen f p1 + cantidadCapasQueCumplen f p2
	
Por inducción en la estructura de p1
- Caso base p = Prepizza
	cantidadCapasQueCumplen f (conCapasDe Prepizza p2) = cantidadCapasQueCumplen f Prepizza + cantidadCapasQueCumplen f p2
- Caso ind p  = Capa i p'
	TI: cantidadCapasQueCumplen f (conCapasDe (Capa i p') p2) = cantidadCapasQueCumplen f (Capa i p') + cantidadCapasQueCumplen f p2?
	HI: cantidadCapasQueCumplen f (conCapasDe p' p2) = cantidadCapasQueCumplen f p' + cantidadCapasQueCumplen f p2!
	
Demostración
- Caso base
	cantidadCapasQueCumplen f (conCapasDe Prepizza p2) 
	= def conCapasDe
	cantidadCapasQueCumplen f (flip (pizzaProcesada (\i rp -> Capa i rp)) Prepizza p2)
	= def flip
	cantidadCapasQueCumplen f (pizzaProcesada (\i rp -> Capa i rp) p2 Prepizza)
	= pizzaProcesada.1
	cantidadCapasQueCumplen f p2
	
	
	cantidadCapasQueCumplen f Prepizza + cantidadCapasQueCumplen f p2
	= cantidadCapasQueCumplen.1
	0 + cantidadCapasQueCumplen f p2
	= aritmetica
	cantidadCapasQueCumplen f p2

Caso ind
	cantidadCapasQueCumplen f (conCapasDe (Capa i p') p2) 
	= def conCapasDe
	cantidadCapasQueCumplen f (flip (pizzaProcesada(\i rp -> Capa i rp)) (Capa i p') p2)
	= def flip
	cantidadCapasQueCumplen f ((pizzaProcesada (\i rp -> Capa i rp) p2 (Capa i p'))
	= pizzaProcesada.2
	cantidadCapasQueCumplen f ((\i rp -> Capa i rp) i (pizzaProcesada (\i rp -> Capa i rp) p2 p'))
	= beta reducción
	cantidadCapasQueCumplen f (Capa i (pizzaProcesada (\i rp -> Capa i rp) p2 p'))
	= def conCapasDe
	cantidadCapasQueCumplen f (Capa i (conCapasDe p' p2))
	=cantidadCapasQueCumplen.2
	if f i then 1 else 0 + cantidadCapasQueCumplen f (conCapasDe p' p2)
	= HI
	if f i then 1 else 0 + cantidadCapasQueCumplen f p' + cantidadCapasQueCumplen f p2
	= cantidadCapasQueCumplen.2
	cantidadCapasQueCumplen f (Capa i p') + cantidadCapasQueCumplen f p2
	
	LLEGUÉ
	
	
c. para todo f. para todo p1 . para todo p2 .
	conCapasTransformadas f (conCapasDe p1 p2) = conCapasDe (conCapasTransformadas f p1) (conCapasTransformadas f p2)
	

Por inducción en la estructura de p1
	
- Caso base p1 = Prepizza
	conCapasTransformadas f (conCapasDe Prepizza p2) = conCapasDe (conCapasTransformadas f Prepizza) (conCapasTransformadas f p2)
- Caso base p1 = Capa i p'
	TI: conCapasTransformadas f (conCapasDe (Capa i p') p2) = conCapasDe (conCapasTransformadas f (Capa i p')) (conCapasTransformadas f p2)
	HI: conCapasTransformadas f (conCapasDe p' p2) = conCapasDe (conCapasTransformadas f p') (conCapasTransformadas f p2)
	
Demostración 
- Caso base
	conCapasTransformadas f (conCapasDe Prepizza p2) 
	= def conCapasDe
	conCapasTransformadas f (flip (pizzaProcesada(\i rp -> Capa i rp)) Prepizza p2)
	= def flip
	conCapasTransformadas f ((pizzaProcesada (\i rp -> Capa i rp)) p2 Prepizza)
	= pizzaProcesada.1
	conCapasTransformadas f p2 
	
	
	conCapasDe (conCapasTransformadas f Prepizza) (conCapasTransformadas f p2)
	= def conCapasTransformadas
	conCapasDe (pizzaProcesada (\i rp -> Pizza (f i) rp) Prepizza Prepizza) (conCapasTransformadas f p2)
	= pizzaProcesada.1
	conCapasDe Prepizza (conCapasTransformadas f p2)
	= def conCapasDe
	flip (pizzaProcesada(\i rp -> Capa i rp)) Prepizza (conCapasTransformadas f p2)
	= def flip
	(pizzaProcesada(\i rp -> Capa i rp)) (conCapasTransformadas f p2) Prepizza
	= pizzaProcesada.1
	conCapasTransformadas f p2
	
- Caso ind
	conCapasTransformadas f (conCapasDe (Capa i p') p2) 
	= def conCapasDe
	conCapasTransformadas f (flip (pizzaProcesada(\i rp -> Capa i rp)) (Capa i p') p2)
	= def flip
	conCapasTransformadas f ((pizzaProcesada (\i rp -> Capa i rp)) p2 (Capa i p'))
	= pizzaProcesada.2
	conCapasTransformadas f ((\i rp -> Capa i rp) i (pizzaProcesada (\i rp -> Capa i rp) p2 p')) 
	= beta reducción
	conCapasTransformadas f (Capa i (pizzaProcesada (\i rp -> Capa i rp) p2 p')) 
	= def conCapasTransformadas
	conCapasTransformadas f (conCapasDe p' p2)
	= HI
	conCapasDe (conCapasTransformadas f p') (conCapasTransformadas f p2)
	
	
	conCapasDe (conCapasTransformadas f (Capa i p')) (conCapasTransformadas f p2)
	= conCapasTransformadas.1
	conCapasDe (pizzaProcesada (\i rp -> Pizza (f i) rp) Prepizza (Capa i p')) (conCapasTransformadas f p2)
	= pizzaProcesada.2
	conCapasDe (f i (pizzaProcesada (\i rp -> Pizza (f i) rp) Prepizza p')) (conCapasTransformadas f p2)
	= def conCapasTransformadas
	conCapasDe (conCapasTransformadas f p') (conCapasTransformadas f p2)
	
	LLEGUÉ

	
d. para todo f. cantidadCapasQueCumplen f . soloLasCapasQue f = cantidadCapasQueCumplen f

Por ppio de extensionalidad sea p una Pizza cualquiera
	(cantidadCapasQueCumplen f . soloLasCapasQue f) p = cantidadCapasQueCumplen f p
	
Por def (.)
	cantidadCapasQueCumplen f (soloLasCapasQue f p) = cantidadCapasQueCumplen f p
	
Por ppio de inducción en la estructura de p
- Caso base p = Prepizza
	cantidadCapasQueCumplen f (soloLasCapasQue f Prepizza) = cantidadCapasQueCumplen f Prepizza
- Caso base p = Capa i p'
	TI: cantidadCapasQueCumplen f (soloLasCapasQue f (Capa i p')) = cantidadCapasQueCumplen f (Capa i p')
	HI: cantidadCapasQueCumplen f (soloLasCapasQue f i p') = cantidadCapasQueCumplen f  p'
	
Demostración
- Caso base
	cantidadCapasQueCumplen f (soloLasCapasQue f Prepizza) 
	= def soloLasCapasQue
	cantidadCapasQueCumplen f (pizzaProcesada (\i rp -> (if f i then Capa i else id) rp ) Prepizza)
	
	cantidadCapasQueCumplen f Prepizza

	
	
pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
pizzaProcesada f g (Prepizza)  = g
pizzaProcesada f g (Capa i p) = f i (pizzaProcesada f g p)

soloLasCapasQue f = pizzaProcesada (\i rp -> (if f i then Capa i else id) rp ) Prepizza

capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen f = pizzaProcesada (\i rp -> if f i then i:rp else rp ) []

conCapasTransformadas f = pizzaProcesada (\i rp -> Pizza (f i) rp) Prepizza

cantidadCapasQueCumplen:: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen f (Prepizza) = 0
cantidadCapasQueCumplen f (Capa i p) =  if f i then 1 else 0 + cantidadCapasQueCumplen f p

conCapasDe :: Pizza -> Pizza -> Pizza -- que agrega las capas de la primera pizza sobre la segunda
conCapasDe p = flip (pizzaProcesada(\i rp -> Capa i rp)) p

-}

-- Ejercicio 7) Definir las siguientes funciones de esquemas sobre listas, utilizando recursión estructural de forma explícita:
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs) = (if f x then (x:) else id)  (filter f xs)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f g []     = g
foldr f g (x:xs) = f x (foldr f g xs)

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr f g []     = f
recr f g (x:xs) = g x xs (recr f g xs)

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f []     = []
foldr1 f (x:xs) = f x (foldr1 f xs)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] []         = []
zipWith f (x:xs) (y:ys) =  f x y : zipWith f xs ys

scanr :: (a -> b -> b) -> b -> [a] -> [b] 
scanr f e []     = [e]
scanr f e (x:xs) =  f x e : (scanr f e xs)

--Ejercicio 8) Demostrar las siguientes propiedades utilizando las definiciones anteriores (y algunas otras de prácticas anteriores):

{-
a. para todo f. para todo g. map f . map g = map (f . g)

Por ppio de extensionalidad sea xs una lista cualquiera
	(map f . map g) xs = map (f . g) xs

Por def (.)
	map f (map g xs) = map f (g xs) 

b. para todo f. para todo xs. para todo ys. map f (xs ++ ys) = map f xs ++ map f ys

Por inducción en la estructura de xs 
- Caso base xs = []
	map f ([] ++ ys) = map f [] ++ map f ys
	
- Caso ind xs = (x:'xs)
	TI: map f ((x:'xs) ++ ys) = map f (x:'xs) ++ map f ys
	HI: map f ('xs ++ ys) = map f 'xs ++ map f ys
	
Demostración
- Caso base
	map f ([] ++ ys) 
	= (++).1
	map f ys
	
	map f [] ++ map f ys
	= map.1
	[] ++ map f ys
	= (++).1
	map f ys
	
- Caso ind
	map f ((x:'xs) ++ ys) 
	= (++).2
	map f (x : (++) 'xs ys)
	= map.2
	f x : map f ((++) 'xs ys)
	= sección opertadores
	f x : map f ('xs ++ ys)
	= HI
	f x : map f 'xs ++ map f ys
	
	
	map f (x:'xs) ++ map f ys
	=map.2
	f x : map f xs ++ map f ys

	LLEGUÉ

c. para todo f. concat . map (map f) = map f . concat

Por ppio de extensionalidad sea xss una lista cualquiera
	(concat . map (map f)) xss = (map f . concat) xss
	
Por def (.)
	concat (map (map f) xss ) = map f (concat xss)
	
Por ppio de inducción en la estructura de xss
- Caso base xss = []
	concat (map (map f) [] ) = map f (concat [])
- Caso ind xss = (xs:'xss)
	TI: concat (map (map f) (xs:'xss) ) = map f (concat (xs:'xss))
	HI: concat (map (map f) 'xss) = map f (concat 'xss)
	
Demostración
- Caso base
	concat (map (map f) [] ) 
	= map.1
	concat []
	= concat.1
	[]
	
	map f (concat [])
	= concat.1
	map f []
	= map.1
	[]
	
- Caso ind
	concat (map (map f) (xs:'xss) ) 
	= map.2
	concat (map f xs : map (map f) 'xss )
	= concat.2
	map f xs ++ concat (map (map f) 'xss)
	= HI
	map f xs ++ map f (concat 'xss)
	= por propiedad practica 11 ejercicio 8a
	map f (xs ++ concat 'xss)
	
	map f (concat (xs:'xss))
	= concat.2
	map f (xs ++ concat 'xss)
	
	LLEGUÉ
	
d. foldr ((+) . suma') 0 = sum . map suma'

Por ppio de extensionalidad sea ps una lista de pares de int cualquiera
	foldr ((+) . suma') 0 ps = (sum . map suma') ps
	
Por def (.)
	foldr ((+) . suma') 0 ps = sum (map suma' ps)

Por ppio de inducción en la estructura de ps 
- Caso base ps = []
	foldr ((+) . suma') 0 [] = sum (map suma' [])
- Caso ind ps = (p:'ps)
	TI: foldr ((+) . suma') 0 (p:'ps) = sum (map suma' (p:'ps))
	HI: foldr ((+) . suma') 0 'ps = sum (map suma' 'ps)
	
Demostración

- Caso base
	foldr ((+) . suma') 0 [] 
	= foldr.1
	0
	
	sum (map suma' [])
	= map.1
	sum []
	= sum.1
	0
	
- Caso ind
	foldr ((+) . suma') 0 (p:'ps) 
	= foldr.2
	((+) . suma') p (foldr ((+) . suma') 0 'ps)
	= HI
	((+) . suma') p (sum (map suma' 'ps))
	= por def (.)
	suma' p + (sum (map suma' 'ps))
	
	sum (map suma' (p:'ps))
	= map.2
	sum (suma' p : map suma' 'ps)
	= sum.2
	suma' p +  sum (map suma' 'ps)
	
	LLEGUÉ


e. para todo f. para todo z. foldr f z . foldr (:) [] = foldr f z
f. para todo f. para todo z. para todo xs. para todo ys.
	foldr f z (xs ++ ys) = foldr f (foldr f z ys) xs
g. (+1) . foldr (+) 0 = foldr (+) 1
h. para todo n. para todo f. many n f = foldr (.) id (replicate n f)
	siendo 
	many 0 f = id
	many n f = f . many (n - 1)
i. para todo f. para todo xs. para todo ys.
	zipWith (f . swap) xs ys = map (uncurry f) (flip zip xs ys)
j. (Desafío) para todo f. para todo g. para todo h. para todo z.
	si para todo x. para todo y. h (f x y) = g x (h y)
	entonces h . foldr f z = foldr g (h z)
		Dar un ejemplo de uso específico de esta propiedad.

-}

-- Ejercicio 9) Definir las siguientes funciones utilizando solamente foldr:

sum :: [Int] -> Int
--sum = foldr (\x rxs -> x + rxs) 0
sum = foldr (+) 0

length :: a -> Int
sum = foldr (\x rxs -> 1 + rxs) 0

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x rxs -> f x : rxs) []

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\x rxs -> (if f x then (x:) else id) rxs) []

find :: (a -> Bool) -> [a] -> Maybe a
find f = foldr (\x rxs -> if f x then Just x else rxs ) Nothing

any :: (a -> Bool) -> [a] -> Bool
--any f = foldr (\x rxs -> f x || rxs) False
any f = foldr ((||) . f) False

all :: (a -> Bool) -> [a] -> Bool
--all f = foldr (\x rxs -> f x && rxs) True
all f = foldr ((&&) . f) True

countBy :: (a -> Bool) -> [a] -> Int
--countBy f = foldr (\x rxs -> if f x then 1+rxs else rxs) 0
--countBy f = foldr (\x rxs -> (if f x then succ else id) rxs) 0
countBy f = foldr (\x rxs -> (if f x then 1 else 0) + rxs) 0

partition :: (a -> Bool) -> [a] -> ([a], [a])
--partition f = foldr (\x rps -> if f x then (x: fst rps, snd rps) else (fst rps, x: snd rps) ) ([], [])
partition f = foldr g ([], [])
 where g x r = if f x then (x: fst rps, snd rps) else (fst rps, x: snd rps)

{-zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f = foldr g h []
 where g r

scanr :: (a -> b -> b) -> b -> [a] -> [b]

takeWhile :: (a -> Bool) -> [a] -> [a]
take :: Int -> [a] -> [a]
drop :: Int -> [a] -> [a]
(!!) :: Int -> [a] -> a
-}

-- Ejercicio 10) Indicar cuáles de las siguientes expresiones tienen tipo, y para aquellas que lo tengan, decir cuál es ese tipo:
{-

a. filter id
b. map (\x y z -> (x, y, z))
c. map (+)
d. filter fst
e. filter (flip const (+))
f. map const
Página 3 de 4Programación Funcional
g. map twice
h. foldr twice
i. zipWith fst
j. foldr (\x r z -> (x, z) : r z) (const [])

-}
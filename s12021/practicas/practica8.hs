-- SECCION I

-- Ejercicio 1) Definir las siguientes funciones sobre listas utilizando recursión estructural (o primitiva de ser necesario):

length::[a]->Int
length [] =  0
length (x:xs) = 1 + length xs

sum::[Int]-> Int
sum [] = 0
sum (x:xs) = x + sum xs

product::[Int]->Int
product [] = 1
product (x:xs) = x * product xs

concat'::[[a]]->[a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

elem'::Eq a=> a-> [a]->Bool
elem' e [] = False
elem' e  (x:xs) = x == e ||  elem' e xs

any'::(a->Bool) -> [a]->Bool
any' p [] = False
any' p  (x:xs) = p x || any' p xs

all'::(a->Bool)-> [a]->Bool
all' p [] = True
all' p  (x:xs) = p x &&  all' p xs

count::(a->Bool)->[a]->Int
count p [] = 0
count p (x:xs) = unoSiCumple p x +  count p xs

unoSiCumple::(a->Bool)->a-> Int
unoSiCumple p e = if p e then 1 else 0

subset::Eq a=> [a]->[a]-> Bool
subset [] ys = True
subset (x:xs) ys = elem' x ys && subset xs ys

(++)::[a]->[a]->[a]
(++) [] ys = ys
(++) (x:xs) ys = x : (++) xs ys

reverse'::[a]->[a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip'::[a]->[b]->[(a,b)]
zip' [x] [y] = [(x, y)] 
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

unzip'::[(a,b)]->([a],[b])
unzip' [] = ([],[])
unzip' (p:ps) = let (xs, ys) = unzip' ps in (fst p:xs, snd p: ys)   

-- Ejercicio 2) Demostrar por inducción estructural las siguientes propiedades:

{-
a. para todo xs. para todo ys. length (xs ++ ys) = length xs + length ys

Sea xs una lista cualquiera
Caso base xs = []         length ([] ++ ​ys​) ​=​ length [] + length ​ys
Caso ind xs = (x:xs')    length (​(x:xs') ++ ​ys​) ​=​ length (x:xs') + length ​ys
        HI: length (​xs' ++ ​ys​) ​=​ length xs' + length ​ys
		
Demostración:
Caso base

	length ([] ++ ​ys​)
	= (++).1
	length ys
	= aritmetica
	0 + length ys
	= length.1
	length [] + length ys
	LLEGUÉ

Caso ind
	length (​(x:xs') ++ ​ys​)
	= (++).2
	length (x : (++) xs' ys)
	= length.2
	1 + length ( (++) xs' ys)
	= HI
	1 + length xs' + length ​ys
	= length.2
	length(xs:xs') + length ​ys
	LLEGUÉ

b. count (const True) = length

Por ppio de extensionalidad en la estructura de la listas, sea xs una lista cualquiera
Caso base xs=[]: count (const True) [] = length []
Caso base xs= (x:xs')
			TI: count (const True) (x:xs') = length (x:xs')?
			HI: count (const True) xs' = length xs'!
			
Demostración

Caso base

	count (const True) []
	= count.1
	0
	= length.1
	length []
	LLEGUÉ
	
Caso ind
	count (const True) (y:ys)
	= count.2
	unoSiCumple (const True) x + count (const True) xs'
	= HI
	unoSiCumple (const True) x + length xs'
	= def unoSiCumple
	if (const True) x then 1 else 0 + length xs'
	= def const
	if True then 1 else 0 + length xs'
	= def if..then..else
	1 + length xs'
	= length.2
	length (x:xs')

c- elem = any . (==)

Por ppio de extensionalidad sea y un elemento cualquiera y ys una lista cualquiera
elem y ys = (any . (==)) y ys?

Por def (.)
elem y ys = any (y==) ys?

Por inducción estructural en la estructura de la listas
Caso base ys = []  
	elem y [] = elem y [] = any (y==) []?
Caso ind ys= (x:xs)
	TI: elem y (x:xs) = any (y==) (x:xs)?
	HI: elem y xs = any (y==) xs!
		
Demostración
Caso base 
	LADO DER
	any (y==) []
	= any.1
	False
	= elem.1
	elem y []
	LLEGUÉ
	
Caso ind
	LADO DER	
	any (y==) (x:xs)
	= any.2
	(y==) x || any (y==) xs
	= HI
	(y==) x || elem y xs
	= sección operadores
	y == x || elem y xs
	= elem.2
	elem y  (x:xs)
	LLEGUÉ
	
d. para todo x. any (elem x) = elem x . concat
Por ppio de extensionalidad
any (elem x) yss = (elem x . concat) yss?

Por def (.)
	any (elem x) yss = elem x (concat yss) ?
	
Por ppio de inducción en la estructura de las listas, sea ys una lista de listas cualquiera
Caso base yss=[]
	any (elem x) [] = elem x (concat []) 
Caso ind ys=(x:xs)
	TI: any (elem x) (xs:xss) = elem x (concat (xs:xss))?
	HI: TI: any (elem x) xs) = elem x (concat xss)!

Demostración
Caso base 
	LADO DER
	elem x (concat [])
	= concat.1
	elem x []
	= elem.1
	False
	
	LADO IZQ
	any (elem x) []
	= any.1
	False
	
	LLEGUÉ
	
Caso ind
	LADO IZQ
	any (elem x) (xs:xss) 
	= any.2
	elem x xs || any (elem x) xss
	= HI
	elem x xs || elem x (concat xss)
	
	LADO DER
	elem x (concat (xs:xss))
	= concat.2
	elem x (xs ++ concat xss)
	= LEMA elem-concat
	elem x xs || elem x (concat xss)
	
	LEMA elem-concat
	elem x (xs ++ xss') = elem x xs || elem x xss'
	
	Caso base xs = []
		elem x ([] ++ xss') = elem x [] || elem x xss'
	Caso ind xs = (x:xs')
		TI: elem x ((x:xs') ++ xss') = elem x (x:xs') || elem x xss'?
		HI: elem x (xs' ++ xss') = elem x xs'  || elem x xss'!
	
	Demostración
	Caso base 
		LADO IZQ
		elem x ([] ++ xss') 	
		= (++).1
		elem x xss'
		
		LADO DER
		elem x [] || elem x xss'
		= elem.1
		False || elem x xss'
		= operadores lógicos
		elem x xss'
		LLEGUÉ
	
	Caso ind
		LADO IZQ
		elem x ((x:xs') ++ xss') 
		= (++).2
		elem x (x: (++) xs' xss')
		= elem.2
		x == x || elem x ((++) xs' xss')
		= sección operadores
		x == x || elem x ( xs' ++ xss')
		
		LADO DER
		elem x (x:xs') || elem x xss'
		= elem.2
		x == x || elem x xs' || elem x xss'
		= HI
		x == x || elem x (xs' ++ xss')
		LLEGUÉ
	
	HI: elem x (xs' ++ xss') = elem x xs'  || elem x xss'!
-}


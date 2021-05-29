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
elem y ys = ((any . (==)) y) ys?

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
	
e. para todo xs. para todo ys. subset xs ys = all (flip elem ys) xs

Por inducción en la estructura de las listas, sea xs una lista cualquiera
Caso base xs=[]
	subset [] ys = all (flip elem ys) []?
Caso ind xs = (x:xs')	
	TI: subset (x:xs') ys = all (flip elem ys) (x:xs')?
	HI: subset xs' ys = all (flip elem ys) xs'!
	
Demostración

Caso base
	LADO DER
	all (flip elem ys) []
	= all.1
	True
	= subset.1
	subset [] ys
	LLEGUÉ
	
Caso ind
	LADO DER
	all (flip elem ys) (x:xs')
	= all.2
	(flip elem ys) x && all (flip elem ys) xs'
	= HI
	(flip elem ys) x && subset xs' ys 
	= def flip
	elem x ys && subset xs' ys 
	= subset.2
	subset (x:xs') ys
	LLEGUÉ

f. all null = null . concat

Por ppio de extensionalidad sea yss una lista cualquiera
	all null yss = (null . concat) yss

Por def (.)
	all null yss = null (concat yss)
	
Por inducción en la estructura de las listas
Caso base yss = []
	all null [] = null (concat [])
Caso ind yss = (xs:xss)
	TI: all null (xs:xss) = null (concat (xs:xss))?
	HI: all null xss = null (concat xss)!

Demostración
Caso base 
	LADO IZQ
	all null []
	= all.1
	True
	
	null (concat [])
	= concat.1
	null []
	= def null
	True

Caso ind
	Lado der
	all null (xs:xss) 
	= all.2
	null xs && all null xss
	= HI
	null xs && null (concat xss)
	
	null (concat (xs:xss))
	= concat.2
	null (xs ++ concat xss)
	= LEMA dist-null
	null xs && null (concat xss)
	
	
	LEMA dist-null 
		null (xs ++ xs') = null xs && null xs'
	Por ppio inducción en la estructura de xs
	Caso base xs = []
		null ([] ++ xs') = null [] && null xs'
	Caso ind xs= (x:xs'')
		TI: null ((x:xs'') ++ xs') = null (x:xs'') && null xs'!
		HI: null (xs'' ++ xs') = null xs'' && null xs'
		
	Demostración
	Caso base
		null ([] ++ xs')
		= (++).1
		null xs'
		= bool
		True && null xs'
		= def null
		null [] && null xs'
		LLEGUÉ
		
	Caso ind
		LADO IZQ
		null ((x:xs'') ++ xs') 
		= (++)
		null (x : (++) xs'' xs')
		= def null
		False
	
		null (x:xs'') && null xss'
		= def null
		False && null xss'
		= bool
		False
		
g. length = length . reverse

Por ppio de extensionalidad sea ys una lista cualquiera 
	length ys = (length . reverse) ys
Por def de (.)
	length ys = length (reverse ys)

Pro ppio de inducción en la estructura de listas
Caso base ys=[]
	length [] = length (reverse [])
Caso ind ys=(x:xs)
	TI: length (x:xs) = length (reverse (x:xs) )?
	HI: length xs = length (reverse xs)!
	
Demostración
Caso base
	LADO DER
	length (reverse [])
	= reverse.1
	length []
	
	LLEGUÉ
	
Caso ind
	LADO DER
	length (reverse (x:xs) )
	= reverse.2
	length (reverse xs ++ [x])
	= LEMA length-(++) demostrado en 2.a
	length reverse xs + length [x]
	= HI
	length xs + length [x]
	= def length
	length xs + 1
	= conmutativa
	1 + length xs
	= length.2	
	length (x:xs) 
	
	LLEGUÉ
	
h. para todo xs. para todo ys.
		reverse (xs ++ ys) = reverse ys ++ reverse xs
		
Por ppio de inducción en la estructura de las listas, sea xs una lista cualquiera
Caso base xs =[]
	reverse ([] ++ ys) = reverse ys ++ reverse []
Caso ind xs =(x:xs')
	TI: reverse ((x:xs') ++ ys) = reverse ys ++ reverse (x:xs')?
	HI: reverse (xs' ++ ys) = reverse ys ++ reverse xs'!
	
Demostración
Caso base
	reverse ([] ++ ys) 
	= (++).1
	reverse ys

	
	reverse ys ++ reverse []
	= reverse.1
	reverse ys ++ []
	= (++).1
	reverse ys
	
Caso ind
	LADO DER
	reverse ys ++ reverse (x:xs')
	= reverse.2
	reverse ys ++ reverse xs' ++ [x]

	
	LADO IZQ
	reverse ((x:xs') ++ ys) 
	= (++).2
	reverse (x : xs' ++ ys)
	= reverse.2
	reverse (xs' ++ ys) ++ [x]
	= HI
	reverse ys ++ reverse xs' ++ [x]
	
	LLEGUÉ

-}


------------------------------------- SECCION II

-- Ejercicio 1: dada la estructura

data N = Z | S N

-- a. definir las funciones

evalN :: N -> Int
evalN Z = 0
evalN (S n) = 1 + evalN n

addN :: N -> N -> N
addN Z n = n
addN (S n) n2 = S (addN n n2)

prodN :: N -> N -> N
prodN Z n =
prodN (S n) n2 = addN n2 (prodN n n2)

int2N :: Int -> N
int2N 0 = Z
int2N n = S (int2N (n-1))





-- b. demostrar
{-

i. para todo n1. para todo n2.
	evalN (addN n1 n2) = evalN n1 + evalN n2
	
Por ppio de inducción en la estructura de N, sea n1 un N cualquiera
Caso base n1 = Z
	evalN (addN Z n2) = evalN Z + evalN n2
	
Caso ind n1 = S n
	TI: evalN (addN (S n) n2) = evalN (S n) + evalN n2?
	HI: evalN (addN n n2) = evalN n + evalN n2!
	
Demostración

Caso base
	evalN (addN Z n2) 
	= addN.1
	evalN n2
	= aritmetica
	0 + evalN n2
	= evalN.1
	evalN Z + evalN n2

Caso ind
	LADO IZQ
	evalN (addN (S n) n2) 
	= addN.2
	evalN (S (addN n n2))
	= evalN
	1 + evalN (addN n n2)
	
	LADO DER
	evalN (S n) + evalN n2
	= evalN.2
	1 + evalN n + evalN n2
	= HI
	1 + evalN (addN n n2)
	

ii. para todo n1. para todo n2.
	evalN (prodN n1 n2) = evalN n1 * evalN n2
	
Por ppio de inducción en la estructura de n1
-Caso base n1 = Z ​   evalN (prodN ​Z​​ n2​) =​ evalN ​Z​ * evalN ​n2
-Caso ind n1 = (S n) evalN (prodN ​(S n) ​​n2​) =​ evalN ​(S n)​ * evalN ​n2
		HI: evalN (prodN ​n ​​n2​)=​ evalN ​n * evalN ​n2
		
Demostración
- Caso base
	evalN (prodN ​Z​​ n2​)
	=prodN.1
	evalN Z
	= evalN.1
	0
	
	evalN ​Z​ * evalN ​n2
	= evalN.1
	0 * evalN ​n2
	= aritmetica
	0
	
- Caso ind
	evalN (prodN ​(S n) ​​n2​)
	= prodN.2
	evalN (addN n2 (prodN n n2))
	= por prop b.i
	evalN n2 + evalN (prodN n n2)
	= HI
	evalN n2 + (evalN ​n * evalN ​n2)

	evalN ​(S n)​ * evalN ​n2
	= evalN.2
	(1 + evalN n) * evalN ​n2
	= aritmetica
	evalN ​n2 + (evalN ​n * evalN ​n2)
	
iii. int2N . evalN = id

Por ppio de extensionalidad sea un n cualquiera
(int2N . evalN) n ​=​ id n
Por def (.)
int2N (evalN n) = id n

Por ppi de inducción en la estructura de N

- Caso base n = Z     
	int2N (evalN Z) = id Z
- Caso ind n = (S n)  
	TI: int2N (evalN (S n)) = id (S n)
	HI: int2N (evalN n) = id n

Demostración
- Caso base
	int2N (evalN Z)
	= evalN.1
	int2N 0
	= int2N.1
	Z
	= def id
	id Z
	
- Caso ind
	int2N (evalN (S n)) 
	= evalN.2
	int2N (1 + evalN n)
	=int2N.1
	S ( int2N (1 + evalN n -1) )
	= aritmetica
	S ( int2N (evalN n ) )
	= HI 
	S ( id n )
	= def id
	S n
	= def id
	id (S n)

iv. evalN . int2N = id

Por ppio de extensionalidad sea n un N cualquiera
	(evalN . int2N ) n = id n
Por def (.)
	evalN (int2N  n) = id n

Por ppio de inducción en la estructura de N, sea n un N cualquiera

-}

int2N::Int->N
int2N 0 = Z
int2N n = S (int2N (n-1))




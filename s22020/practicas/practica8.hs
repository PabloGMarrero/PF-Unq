-----Seccion I

--Ej1)

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

{-Ej2
a) para todo ​xs​. para todo ​ys​.​length (​xs​ ++ ​ys​) ​=​ length ​xs​ + length ​ys

sea xs una lista cualquiera
Caso base xs = []         length ([] ++ ​ys​) ​=​ length [] + length ​ys
Caso ind xs = (x:xs')    length (​(x:xs') ++ ​ys​) ​=​ length (x:xs') + length ​ys
        HI: length (​xs' ++ ​ys​) ​=​ length xs' + length ​ys

Demostración
-Caso base
    length ([] ++ ​ys​)
    = (++).1
    length ys


    length [] + length ​ys
    = length.1
    0 + length ys
    = aritmetica
    length ys

-Caso ind
    length (​(x:xs') ++ ​ys​) ​=​ length (x:xs') + length ​ys

    length (​(x:xs') ++ ​ys​)
    = (++).2
    length (x : (++) xs ys)
    = length.2
    1 + length ((++) xs ys)
    = HI
    1 + length xs' + length ​ys

    length (x:xs') + length ​ys
    = length.2
    1 + length xs' + length ys



b.count (const True) ​=​ length
Por ppio de extensionalidad sea xs una lista
count (const True) xs ​=​ length xs

Sea xs una lista cualquiera

-Caso base []   count (const True) [] ​=​ length []
-Caso ind (x:xs')  count (const True) (x:xs') ​=​ length (x:xs')
    TI: count (const True) xs' ​=​ length xs'

Demostración

-Caso base
    count (const True) []
    = count.1
    0
    = length.1
    length []

-Caso ind
    count (const True) (x:xs')
    = count.2
    unoSiCumple (const True x ) + count (const True) xs'
    =def const
    unoSiCumple True + count (const True) xs'
    = def unoSiCumple
    1 + count (const True) xs'
    = HI
    1 + length xs'
    
    length (x:xs')
    = length.2
    1 + length xs'

c) elem ​=  ​any . (==)
Por ppio de extensionalidad sea e un elemento cualquiera y xs una lista cualquiera
elem e ​xs =  ​(any . (==)) e  xs

Por def (.)
elem e ​xs =  ​any (==e) xs

-Caso base []   elem e [] =  ​any (==e)  []
-Caso ind (x:xs')  elem e ​(x:xs') =  ​any (==e) (x:xs')
    TI: elem ​xs' e =  ​any (==e)  xs'

Demostración
-Caso base
    elem e [] 
    = elem.1
    False

    any (==e) []
    =any.1
    False

-Caso ind
    elem e ​(x:xs')
    =elem.2
    e == x || elem e xs'
    
    any (==e) (x:xs')
    =any.2
    (==e) x || any (==e) xs'
    = 
    e == x || any (==e) xs'
    =HI
    e == x || elem e xs'

d)para todo ​x​. ​any (elem ​x​) ​=​ elem ​x​ . concat
Por ppio de extensionalidad sea xs una lista cualquiera
​any (elem ​x​) xs ​=​ (elem ​x​ . concat) xs

Por def (.)
​any (elem ​x​) xs ​=​ elem ​x​ (concat xs)

Por ppio de inducción en la estructura de la listas

-Caso base xs = []  ​​any (elem ​x​) [] ​=​ elem ​x​ (concat [])
-Caso ind  xs = (xs':xss)   any (elem ​x​) (xs':xss)  ​=​ elem ​x​ (concat (xs':xss) )
        TI any (elem ​x​) xss  ​=​ elem ​x​ (concat xss)

Demostración

-Caso base
    any (elem ​x​) []
    =any.1
    False

    elem ​x​ (concat [])
    =concat.1
    elem x []
    =elem.1
    False

-Caso ind
    any (elem ​x​) (xs':xss) 
    =any.2
    elem x xs' || any (elem ​x​) xss
    = HI
    elem x xs' || elem ​x​ (concat xss)
    
    elem ​x​ (concat (xs':xss) )
    = concat.2
    elem x (xs ++ concat xss)
    = LEMA 1
    elem x xs' || any (elem ​x​) xss
    
    Lema 1
    elem x xs' || elem ​x​ xss' = elem x (xs ++ xss')

    Por ppi de inducción en la estructura de xss'
    -Caso base xss' = []   elem x xs || elem ​x​ [] = elem x (xs ++ [])
    -Caso ind xss' = (xs':xss'')  elem x xs' || elem ​x​ (xs':xss'') = elem x (xs ++ (xs':xss''))
                    TI elem x xs' || elem ​x​ xss'' = elem x (xs ++ xss'')

    Demostración
    -Caso base
        elem x xs || elem ​x​ []
        = elem.1
        elem x xs || False
        = boolean
        elem x xs

        elem x (xs ++ [])
        = (++).1
        elem x xs
    -Caso ind
        elem x xs' || elem ​x​ (xs:xss'')
        =elem.2
        elem x xs' || (x == xs || elem x xss'')
        = bool
        elem x xs' || x == xs || elem x xss''
        = conmutativa
        elem x xs' || elem x xss'' || x == xs
        = HI
        elem x (xs ++ xss'') || x == xs
        

        elem x (xs ++ (xs':xss''))
        = (++).2
        elem x (xs' : (++) xs xss'')
        = elem.2
        x == xs' || elem x ((++) xs xss'')
        = conmutativa e infijo
        elem x (xs ++ xss'') || x == xs'

e)para todo ​xs​. para todo ​ys​. ​subset ​xs ​​ys​​ =​ all (flip elem ​ys​) ​xs

Por ppio de inducción en la estructura de la listas

- Caso base xs = []    subset ​[] ​​ys​​ =​ all (flip elem ​ys​) ​[]
- Caso base xs = (x:xs')    subset ​(x:xs') ​​ys​​ =​ all (flip elem ​ys​) ​(x:xs')
			TI subset ​xs' ​​ys​​ =​ all (flip elem ​ys​) xs'

Demostración
-Caso base
	subset ​[] ​​ys​​ 
	=​ subset.1
	True
	
	all (flip elem ​ys​) ​[]
	=all.1
	True
	
-Caso ind
	subset ​(x:xs') ​​ys​​ 
	=subset.2
	elem x ys && subset xs' ys
	= HI
	elem x ys && all (flip elem ​ys​) xs'
	
	​all (flip elem ​ys​) ​(x:xs')
	=all.2
	(flip elem ​ys​) x && all (flip elem ​ys​) xs'
	=def flip
	elem x ys && all (flip elem ​ys​) xs'

f) all null ​=​ null . concat
Por ppio de extensionalidad sea xss una lista cualquiera

all null xss ​=​ (null . concat) xss
Por def (.)
all null xss ​=​ null (concat xss) 

Por ppi de inducción en las listas

- Caso base xss = []    all null [] ​=​ null (concat []) 
- Caso ind xss = (xs:xss')   all null (xs:xss') ​=​ null (concat (xs:xss')) 
			HI: all null xss' ​=​ null (concat xss') 

Demostración

- Caso base
	all null []
	= all.1
	True
	
	null (concat [])
	=concat.1
	null []
	=null.1
	True
	
- Caso ind
	all null (xs:xss')
	=all.2
	null xs && all null xss'
	= HI
	null xs && null (concat xss')
	
	null (concat (xs:xss')) 
	=concat.2
	null (xs ++ concat xss')
	= LEMA 2
	null xs ++ null (concat xss')
	
	LEMA 2 
	null (xs ++ xss'') = null xs && null xss''
	
	-Caso xs = []
		null ([] ++ xss'') 
		= ++.1
		null xss''
		
		null [] && null xss''
		= null.1
		null [] && xss''
		= identidad booleana
		null xss''
		
	-Caso xs = (zs:zss)
		null (zs:zss) ++ xss'') = null (zs:zss) && null xss''
		
		null ((zs:zss) ++ xss'')
		= ++.2
		null (zs : (++) xss'' zss)
		= null.2
		False
		
		null (zs:zss) && null xss''
		= null.1
		False && null xss''
		= def &&
		False
	

g) length ​=​ length . reverse
Por ppio de extensionalidad sea xs una lista cualquiera 
length ​xs =​ length . reverse) xs
Por def (.)
length ​xs =​ length (reverse xs)

Por ppio de inducción en las listas

- Caso base []      length ​[] =​ length (reverse [])
- CAso ind (x:xs')  length ​(x:xs') =​ length (reverse (x:xs'))
			HI: length ​xs' =​ length (reverse xs')
			
Demostración
-Caso base
	Lado der
	
	length (reverse [])
	=reverse.1
	length []

Caso ind
	length ​(x:xs')
	= length.2
	1 + length xs'
	= HI
	1 + length (reverse xs') 
	
	length (reverse (x:xs'))
	=reverse.2
	length (reverse xs' ++ [x])
	= LEMA 1
	length (reverse xs') + length [x]
	= length.2
	length (reverse xs') + 1 + length []
	= length.1
	length (reverse xs') + 1 + 0
	= aritmetica y conmutativa
	1 + length (reverse xs')
	
	Lema
	length (ys ++ [x]) = length ys + length [x]
	
	Por inducción en la estructura de ys
	- Caso base ys = []    length ([] ++ [x]) = length [] + length [x]
	- Caso ind ys= (y:ys)  length ((y:ys) ++ [x]) = length (y:ys) + length [x]
				HI: length (ys ++ [x]) = length ys + length [x]
				
	Demostración
	- Caso base
		length ([] ++ [x])
		= (++).1
		length [x]
		
		length [] + length [x]
		= length.1
		0 + length [x]
		= aritmetica
		length [x]


	- Caso ind
		length ((y:ys) ++ [x]) 
		= (++).2 
		length (y : (++) ys [x])
		= length.2
		1 + length ((++) ys [x])
		
		length (y:ys) + length [x]
		= length.2
		1 + length ys + length [x]
		= HI
		1 + length (ys ++ [x])
		= notación infija
		1 + length ((++) ys [x])
		
		length (ys ++ [x]) = length ys + length [x]
		
i)para todo ​xs​. para todo ​ys​ ​ all p (​xs​++​ys​) ​=​ all p (reverse ​xs​) && all p (reverse ​ys​)

Por inducción en la estructura de las listas
- Caso base xs = []      all p (​[]​++​ys​) ​=​ all p (reverse ​[]​) && all p (reverse ​ys​)
- Caso ind xs = (x:xs')  all p (​(x:xs')​++​ys​) ​=​ all p (reverse ​(x:xs')​) && all p (reverse ​ys​)
		HI: all p (​xs' ++​ys​) ​=​ all p (reverse ​xs'​) && all p (reverse ​ys​)
		
Demostración
-caso base
	all p (​[]​++​ys​)
	=++.1
	all p ys

	all p (reverse ​[]​) && all p (reverse ​ys​)
	=reverse.1
	all p [] && all p (reverse ​ys​)
	=all.1
	True && all p (reverse ​ys​)
	= identidad booleana
	all p (reverse ​ys​)
	= Lema all
	all p ys
	
	Lema all
	all p (reverse ​ys​) = all p ys
	
	Por inducción en la estructura de ys
	- Caso base ys = []    all p (reverse ​[]) = all p []
	- Caso ind ys = (y:ys) all p (reverse ​(y:ys)) = all p (y:ys)
				HI: all p (reverse ​ys) = all p ys
	
	Demostración
	- Caso base
		all p (reverse ​[])
		= reverse.1
		all p []
		
	- Caso ind
		all p (reverse ​(y:ys))
		= reverse.2
		all p (reverse ys ++ [y])

		all p (y:ys)
		=all.2
		p y && all p ys
		= HI
		p y && all p (reverse ​ys)

-}

-- Sección II

data N = Z | S N

--Ej1 a

evalN::N->Int
evalN Z = 0
evalN (S n) = 1 + evalN n

addN::N->N->N
addN Z n = n
addN (S n) n2 = S (addN n n2)

prodN::N->N->N
prodN Z n = Z
prodN (S n1) n2 = addN n2 (prodN n1 n2)

int2N::Int->N
int2N 0 = Z
int2N n = S (int2N (n-1))

{-Demostraciones

i)para todo ​n1​. para todo ​n2​.​evalN (addN ​n1​​ n2​) ​=​ evalN ​n1​ + evalN ​n2

Por ppio de inducción en la estructura de n1
-Caso base n1 = Z ​   evalN (addN ​Z​​ n2​)=​ evalN ​Z​ + evalN ​n2
-Caso ind n1 = (S n) evalN (addN ​(S n) ​​n2​)=​ evalN ​(S n)​ + evalN ​n2
		HI: evalN (addN ​n ​​n2​)=​ evalN ​n + evalN ​n2
		
Demostración
- Caso base
	evalN (addN ​Z​​ n2​)
	= addN.1	
	evalN n2
	=aritmetica
	0 + evalN n2
	=evalN.1
	evalN Z + evalN n2
	
- Caso ind
	evalN (addN ​(S n) ​​n2​)
	= addN.2
	evalN (S (addN n n2))
	= evalN.2
	1 + evalN (addN n n2)
	
	evalN ​(S n)​ + evalN ​n2
	= evalN.2
	1 + evalN n + evalN ​n2
	= HI
	1 + evalN (addN ​n ​​n2​)
	
ii)para todo ​n1​. para todo ​n2​. ​evalN (prodN ​n1 ​​n2​) ​=​ evalN ​n1​ * evalN ​n2

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
	
iii)int2N . evalN ​=​ id

Por ppio de extensionalidad sea un n cualquiera
(int2N . evalN) n ​=​ id n
Por def (.)
int2N (evalN n) = id n

Por ppi de inducción en la estructura de N

- Caso base n = Z     int2N (evalN Z) = id Z
- Caso ind n = (S n)  int2N (evalN (S n)) = id (S n)
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
	???TODO
	
	
	id (S n)

-}

-- Ej 2

type NU = [()]

evalNU::NU ->Int
evalNU [] = 0
evalNU (u:us) = 1 + evalNU us

succNU::NU -> NU
succNU [] = [()]
succNU (u:us) = u: (succNU us)


addNU::NU -> NU -> NU
addNU [] nu     = nu
addNU (u:us) nu = u : addNU us nu 

nu2n::NU->N
nu2n []     = Z
nu2n (u:us) = S(nu2n us)

n2nu::N->NU
n2nu Z = []
n2nu (S n) = () : n2nu n


{-
Demostraciones

i) evalNU . succNU ​=​ (+1) . evalNU
Por ppio de extensionalidad sea nu un NU cualquiera
(evalNU . succNU) nu ​=​ (+1) . evalNU) nu
Por def (.)

evalNU (succNU nu) ​=​ (+1) (evalNU nu)


Por ppio de inducción en la estructura de nu
- Caso base nu = []    evalNU (succNU []) ​=​ (+1) (evalNU [])
- Caso ind nu = (u:us) evalNU (succNU (u:us)) ​=​ (+1) (evalNU (u:us))
		HI: evalNU (succNU us) ​=​ (+1) (evalNU us)

Demostración

- Caso base
	evalNU (succNU []) ​
	=​ succNU.1
	evalNU [()]
	= evalNU.2
	1 + evalNU []
	= notación infija	
	(+1) (evalNU [])

- Caso ind
	evalNU (succNU (u:us))
	= succNU.2
	evalNU (u: succNU us)
	= evalNU.1
	1 + evalNU (succNU us)
	= HI
	1 + ((+1) (evalNU us))
	= notación infija
	1 + 1 + evalNU us

	(+1) (evalNU (u:us))
	= notación infija
	1 + evalNU (u:us)
	= evalNU.2
	1 + 1 + evalNU us


ii)para todo ​n1​.n2​ ​evalNU (addNU ​n1 ​​n2​) ​=​ evalNU ​n1​ + evalNU ​n2

Por ppio de inducción en la estructura de n1
- Caso base n1 = []    evalNU (addNU ​[] ​​n2​) ​=​ evalNU ​[]​ + evalNU ​n2
- Caso ind n1 = (u:us) evalNU (addNU ​(u:us) ​​n2​) ​=​ evalNU ​(u:us)​ + evalNU ​n2
			HI evalNU (addNU ​us ​​n2​) ​=​ evalNU ​us​ + evalNU ​n2

Demostración

- Caso base
	evalNU (addNU ​[] ​​n2​)
	= addNU.1
	evalNU nu
	= aritmetica
	0 + evalNU nu
	= evalNU.1
	evalNU [] + evalNU nu
	
- Caso ind
	evalNU (addNU ​(u:us) ​​n2​)
	= addNU.2
	evalNU (u : addNU us n2)
	= evalNU.2
	1 + evalNU (addNU us n2)
	= HI
	1 + evalNU ​us​ + evalNU ​n2
	= evalNU.2	
	evalNU ​(u:us)​ + evalNU ​n2
	
iii) nu2n . n2nu ​=​ id

Por ppio de extensionalidad sea n un N cualquiera
(nu2n . n2nu) n = id n
Por def (.)
nu2n (n2nu n) = id n

Por ppio de inducción en la estructura de n
- Caso base n = Z    nu2n (n2nu Z) = id Z
- Caso ind n = (S n) nu2n (n2nu (S n')) = id (S n')
			HI: nu2n (n2nu n') = id n'

Demostración
- Caso base 
	nu2n (n2nu Z)
	= n2nu.1
	nu2n []
	= nu2n.1
	Z
	= def id
	id Z
	
- Caso ind
	nu2n (n2nu (S n'))
	= n2nu.2
	nu2n (() : n2 n')
	=


-}

data DigBin = O | I

dbAsInt::DigBin->Int
dbAsInt O = 0
dbAsInt I = 1

dbAsBool::DigBin->Bool
dbAsBool O = False
dbAsBool I = True

dbOfBool::Bool->DigBin​
dbOfBool False = O
dbOfBool True = I

negDB::DigBin->DigBin
negDB O = I
negDB I = O


type NBin = [DigBin]

evalNB::NBin->Int
evalNB [] = 0
evalNB (d:ds) = addDB d (2 * evalNB ds) --el 2*es para desplazar la potencia

addDB::DigBin->Int->Int
addDB O n = n
addDB I n = 1 + n

normalizarNB::NBin->NBin
normalizarNB [] = []
normalizarNB (d:ds) = norm d (normalizarNB ds)

norm::DigBin->NBin->NBin
norm O [] = []
norm d ds = d: ds

succNB::NBin->NBin
succNB [] = [I]
succNB (O:ds) = I: ds
succNB (I:ds) = O: succNB ds --sumo uno por el carry siguiente

addNB::NBin->NBin->NBin
addNB [] m = m
addNB n [] = n
addNB (n:ns) (m:ms) = addDbs n m (addNB ns ms)

addDbs::DigBin->DigBin->NBin->NBin
addDbs O O ds = O: ds
addDbs O I ds = I: ds
addDbs I O ds = I: ds
addDbs I I ds = O : succNB ds

nb2b::NBin->N
nb2b [] =  Z
nb2b (d:ds) = S (nb2b ds) 

n2nb::N->NBin
n2nb Z = []
n2nb (S n) = succNB (n2nb n)


{-
Demostraciones

i) evalNB . normalizarNB ​=​ evalNB
Porp ppio de extensionalidad sea nb un NBin cualquiera

(evalNB . normalizarNB) nb ​=​ evalNB nb

Por def (.)
evalNB (normalizarNB nb) ​=​ evalNB nb


Porp ppio de inducción en la estructura de nb
- Caso base nb = []    evalNB (normalizarNB []) ​=​ evalNB []
- Caso ind nb = (d:ds) evalNB (normalizarNB (d:ds)) ​=​ evalNB (d:ds)
		HI: evalNB (normalizarNB ds) ​=​ evalNB ds

Demostración

-Caso base
	evalNB (normalizarNB [])
	= normalizarNB.1
	evalNB []
	
-Caso ind
	evalNB (normalizarNB (d:ds)) ​
	= normalizarNB.2
	evalNB (norm d (normalizarNB ds) )​ 
	= lema norm-evalNB
	evalNB (d : normalizarNB ds)
	= evalNB
	addDB d (2 * evalNB (normalizarNB ds))
	= HI
	addDB d (2* evalNB ds)
	
	evalNB (d:ds)
	= evalNB.2
	addDB d (2* evalNB ds)
	
	
	Lema norm-evalNB
	evalNB (norm d ds) = evalNB (d : ds)
	
	- Caso 1 d= O ds = []
	- Caso 2 d=d' ds= ds'
	
	Demostración
	- Caso 1
		evalNB (norm O []) 
		= norm.1
		evalNB []
		= evalNB.1
		0
		
		evalNB (O : [])
		=evalNB.2
		addDB O (2 * evalNB [])
		=evalNB.1
		addDB O (2 * 0)
		=aritmetica 
		addDB O 0
		= addDB.1
		0
		
	- Caso 2
		evalNB (norm d' ds') 
		= norm.2
		evalNB (d':ds')
		
ii)evalnNB . succNB ​=​ (+1) . evalNB
Por ppio de extensionalidad sea nb un NBin cualquiera
 evalnNB . succNB) ​nb =​ ((+1) . evalNB) nb
Por def (.)
 evalnNB (succNB ​nb) =​ (+1) (evalNB nb)
 
Por ppio de inducción en la estructura de nb

-Caso base nb = []    evalnNB (succNB ​[]) =​ (+1) (evalNB [])
-Caso ind nb = (d:ds)  evalnNB (succNB ​(d:ds))  =​ (+1) (evalNB (d:ds) )
		HI: evalnNB (succNB ​ds)  =​ (+1) (evalNB ds)

Demostración

- Caso base
	evalNB (succNB ​[])
	=succNB.1​ 
	evalNB [I]
	=evalNB.2
	addDB I (2 * evalNB [])
	= evalNB.1
	addDB I (2 * 0)
	= aritmetica
	addDB I 0
	=addDB.
	1 + 0
	
	
	(+1) (evalNB [])
	= evalNB.1
	(+1) 0
	= notación infija
	1 + 0
	
- Caso ind
	evalnNB (succNB ​(d:ds))  
	=​ succNB.2
	evalNB ()
	
	(+1) (evalNB (d:ds))
	= evalNB.2
	(+1) (addDB d (2 * evalnNB ds))
	=
	
evalnNB (succNB ​ds)  =​ (+1) (evalNB ds)


iii.para todo ​n1​. para todo ​n2​.​evalNB (addNB ​n1 ​​n2​) ​=​ evalNB ​n1​ + evalNB ​n2

Por ppio de inducción en la estructura de n1 y n2
- Caso base 1 n1 = [] ​evalNB (addNB ​[] ​​n2​) ​=​ evalNB ​[]​ + evalNB ​n2
- Caso base 2 n2 = [] ​evalNB (addNB ​n1 ​​[]​) ​=​ evalNB ​n1​ + evalNB ​[]
- Caso ind n1 = (n:ns) n2 = (m:ms)
		TI: ​evalNB (addNB ​(n:ns) ​​(m:ms)​) ​=​ evalNB ​(n:ns)​ + evalNB ​(m:ms)
		HI: ​evalNB (addNB ​ns ​​ms​) ​=​ evalNB ​ns​ + evalNB ​ms

Demostración

- Caso base 1
	​evalNB (addNB ​[] ​​n2​)
	= addNB.1
	evalNB n2
	= aritmetica
	0 + evalNB n2
	
	evalNB ​[]​ + evalNB ​n2
	= evalNB.1
	0 + evalNB ​n2
	
- Caso base 2
	​evalNB (addNB ​n1 [])
	= addNB.2
	evalNB n1
	= aritmetica neutro suma
	evalNB n1 + 0
	
	evalNB ​n1​ + evalNB ​[]
	= evalNB.1
	evalNB ​n1 + 0
	
- Caso ind
	evalNB (addNB ​(n:ns) ​​(m:ms)​) 
	​=​ addNB.3
	evalNB (addDB...)
	
	evalNB ​(n:ns)​ + evalNB ​(m:ms)
	
	

iv) nb2n . n2nb ​=​ id

Pr ppio de extensionalidad sea n cualquier N 
 nb2n . n2nb) n ​=​ id n
Por def (.)
 nb2n (n2nb n) ​=​ id n
 
Por ppio de inducción en la estructura de n1
- Caso base n1= Z​      nb2n (n2nb Z) ​=​ id Z
- Caso ind n1 = (S n)  nb2n (n2nb (S n) ​=​ id (S n)
					HI: nb2n (n2nb n) ​=​ id n
					
Demostración
- Caso base
	nb2n (n2nb Z) ​
	= n2nb.1
	nb2n []
	=nb2n.1
	Z
	= def id
	​id Z

- Caso ind
	nb2n (n2nb (S n)
	=n2nb.2
	nb2n (succNB (n2nb n))
	= 
	

-}

--Seccion III

--Ej 1

data ExpA =Cte Int | Suma ExpA ExpA | Prod ExpA ExpA deriving Show 

evalExpA::ExpA->Int
evalExpA (Cte n) = n
evalExpA (Suma e1 e2) = evalExpA e1 + evalExpA e2
evalExpA (Prod e1 e2) = evalExpA e1 * evalExpA e2

simplificarExpA::ExpA->ExpA
simplificarExpA (Cte n) = Cte n
simplificarExpA (Suma e1 e2) = simpSumaExpA (simplificarExpA e1) (simplificarExpA e2)
simplificarExpA (Prod e1 e2) = simpProdExpA (simplificarExpA e1) (simplificarExpA e2)

simpSumaExpA::ExpA->ExpA-> ExpA
simpSumaExpA (Cte 0) e2 = e2
simpSumaExpA e1 (Cte 0) = e1

simpProdExpA::ExpA->ExpA-> ExpA
simpProdExpA (Cte 0) e2 = (Cte 0)
simpProdExpA (Cte 1) e2 = e2
simpProdExpA e1 (Cte 0) = (Cte 0)
simpProdExpA e1 (Cte 1) = e1

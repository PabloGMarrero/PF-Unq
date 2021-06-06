data ExpA =Cte Int | Suma ExpA ExpA | Prod ExpA ExpA deriving Show 
data EA = Const Int | BOp BinOp EA EA  deriving Show   
data BinOp = Sum | Mul deriving Show

exp = Suma (Cte 1) (Cte 2)
ea = BOp Sum () ()

evalEA::EA->Int	
evalEA (Const n) = n
evalEA (BOp b e1 e2) = evalBOp b(evalEA e1) (evalEA e2)

evalBOp::BinOp->Int->Int->Int
evalBOp Sum = (+)
evalBOp Mul = (*)

ea2ExpA::EA->ExpA​
ea2ExpA (Const n) = Cte n
ea2ExpA (BOp b e1 e2) = bop2ExpA b (ea2ExpA e1) (ea2ExpA e2)

bop2ExpA::BinOp->ExpA->ExpA->ExpA
bop2ExpA Sum = Suma
bop2ExpA Mul = Prod

expA2ea::ExpA->EA​
expA2ea (Cte n) = Const n
expA2ea (Suma e1 e2) = BOp Sum (expA2ea e1) (expA2ea e2)
expA2ea (Prod e1 e2) = BOp Mul (expA2ea e1) (expA2ea e2)

{-
Demostraciones
i) ea2ExpA . expA2ea ​=​ id
Por ppio de extensionalidad sea ex un ExpA cualquiera
ea2ExpA . expA2ea) e1x ​=​ id ex
Por def (.)
ea2ExpA (expA2ea ex) ​=​ id ex

Por ppio de inducción en e1
- Caso base ex = Const n    
	ea2ExpA (expA2ea (Cte n)) ​=​ id (Cte n)
- Caso ind1 ex = Suma e1 e2 
	TI: ea2ExpA (expA2ea (Suma e1 e2)) ​=​ id (Suma e1 e2)
	HI1: ea2ExpA (expA2ea e1) ​=​ id e1
	HI2: ea2ExpA (expA2ea e2) ​=​ id e2
- Caso ind2 ex = Prod e1 e2 
	TI: ea2ExpA (expA2ea (Prod e1 e2)) ​=​ id (Prod e1 e2)
	HI1: ea2ExpA (expA2ea e1) ​=​ id e1
	HI2: ea2ExpA (expA2ea e2) ​=​ id e2
	
Demostración
- Caso base
	ea2ExpA (expA2ea (Cte n)) 
	= expA2ea.1
	ea2ExpA (Const n)
	= ea2ExpA.1
	Cte n
	= def id
	id (Cte n)
	
- Caso ind1
	ea2ExpA (expA2ea (Suma e1 e2))
	= expA2ea.2 
	ea2ExpA (BOp Sum (expA2ea e1) (expA2ea e2))
	= ea2ExpA.2
	bop2ExpA Sum (ea2ExpA (expA2ea e1)) (ea2ExpA (expA2ea e2))
	= bop2ExpA.1
	Suma (ea2ExpA (expA2ea e1)) (ea2ExpA (expA2ea e2))
	= HI1
	Suma (id e1) (ea2ExpA (expA2ea e2))
	= HI2
	Suma (id e1) (id e2)
	= def id x2
	Suma e1 e2
	= def 
	id (Suma e1 e2)
	
- Caso ind2
	Es exactamente que caso ind1
ii) expA2ea . ea2ExpA ​=​ id
Por ppio de extensionalidad sea ea un EA cualquiera
(expA2ea . ea2ExpA)) ea ​=​ id ea
Por def (.)
expA2ea (ea2ExpA ea) ​=​ id ea

Por ppio de inducción en la estructura de ea
- Caso base ea = Const n
	expA2ea (ea2ExpA (Const n)) ​=​ id (Const n)
- Caso ind ea = BOp b e1 e2 
	TI: expA2ea (ea2ExpA (BOp b e1 e2)) ​=​ id (BOp b e1 e2)
	HI1: expA2ea (ea2ExpA e1) ​=​ id e1
	HI2: expA2ea (ea2ExpA e2) ​=​ id e2
	
Demostración
- Caso base 
	expA2ea (ea2ExpA (Const n))
	=ea2ExpA.1
	expA2ea (Cte n)
	= expA2ea.1
	Const n
	= def id
	id (Const n)
	
- Caso ind
	expA2ea (ea2ExpA (BOp b e1 e2))
	=ea2ExpA.2
	expA2ea (bop2ExpA b (ea2ExpA e1) (ea2ExpA e2)) 
	
	Caso b= Sum
		expA2ea (bop2ExpA Sum (ea2ExpA e1) (ea2ExpA e2)) 
		= bop2ExpA.1
		expA2ea (Suma (ea2ExpA e1) (ea2ExpA e2))
		=expA2ea.2
		BOp Sum (expA2ea (ea2ExpA e1)) (expA2ea (ea2ExpA e2))
		= HI1
		BOp Sum (id e1) (expA2ea (ea2ExpA e2))
		= HI2
		BOp Sum (id e1) (id e2)
		= def id x2
		BOp Sum e1 e2
		= def id 
		id (BOp Sum e1 e2)
		
	Caso b = Mul 
		expA2ea (bop2ExpA Mul (ea2ExpA e1) (ea2ExpA e2)) 
		= bop2ExpA.1
		expA2ea (Prod (ea2ExpA e1) (ea2ExpA e2))
		=expA2ea.2
		BOp Mul (expA2ea (ea2ExpA e1)) (expA2ea (ea2ExpA e2))
		= HI1
		BOp Mul (id e1) (expA2ea (ea2ExpA e2))
		= HI2
		BOp Mul (id e1) (id e2)
		= def id x2
		BOp Mul e1 e2
		= def id 
		id (BOp Mul e1 e2)
	
iii)evalExpA . ea2ExpA ​=​ evalEA
Pr ppio de extensionalidad sea ea un EA cualquiera
evalExpA . ea2ExpA) ea ​=​ evalEA ea
Por def (.)
evalExpA (ea2ExpA ea) ​=​ evalEA ea


Por ppio de inducción en la estructura de ea
- Caso base ea = Const n
	evalExpA (ea2ExpA (Const n)) ​=​ evalEA (Const n)
- Caso ind ea = BOp b e1 e2
	TI: evalExpA (ea2ExpA (BOp b e1 e2)) ​=​ evalEA (BOp b e1 e2)
	HI1:evalExpA (ea2ExpA e1) ​=​ evalEA e1
	HI2:evalExpA (ea2ExpA e2) ​=​ evalEA e2
	
Demostración	
- Caso base
	evalExpA (ea2ExpA (Const n))
	= ea2ExpA.1
	evalExpA (Cte n)
	= evalExpA.1
	n
	
	​evalEA (Const n)
	= evalEA.1
	n
	
- Caso ind
	evalExpA (ea2ExpA (BOp b e1 e2))
	= ea2ExpA.2
	evalExpA (bop2ExpA b (ea2ExpA e1) (ea2ExpA e2))

	Caso b = Sum
		evalExpA (bop2ExpA Sum (ea2ExpA e1) (ea2ExpA e2))
		=bop2ExpA.1
		evalExpA (Suma (ea2ExpA e1) (ea2ExpA e2))
		= evalExpA.2
		evalExpA (ea2ExpA e1) + evalExpA (ea2ExpA e2)
		
		​evalEA (BOp Sum e1 e2)
		=​ evalEA.2
		evalBOp Sum (evalEA e1) (evalEA e2)
		=HI1
		evalBOp Sum (evalExpA (ea2ExpA e1)) (evalEA e2)
		=HI2
		evalBOp Sum (evalExpA (ea2ExpA e1)) (evalExpA (ea2ExpA e2))
		= evalBOp.1
		(+) (evalExpA (ea2ExpA e1)) (evalExpA (ea2ExpA e2))
		= sección operadores
		evalExpA (ea2ExpA e1) + evalExpA (ea2ExpA e2)
	
	Caso b = Mul
		Es equivalente al anterior pero en lugar de Sum es Mul
		
iv) evalEA . expA2ea ​=​ evalExpA
Por ppio de extensionalidad sea ex un ExpA cualquiera
(evalEA . expA2ea) ex ​=​ evalExpA ex
Por def (.)
evalEA (expA2ea ex) ​=​ evalExpA ex

Por ppio de inducción en la estructura de ex
- Caso base ex = Cte n
	evalEA (expA2ea (Cte n)) ​=​ evalExpA (Cte n)
- Caso ind1 ex = Suma e1 e2
	TI: evalEA (expA2ea (Suma e1 e2)) ​=​ evalExpA (Suma e1 e2)
	HI1:evalEA (expA2ea e1) ​=​ evalExpA e1
	HI2:evalEA (expA2ea e2) ​=​ evalExpA e2
- Caso ind2 ex = Prod e1 e2
	TI: evalEA (expA2ea (Prod e1 e2)) ​=​ evalExpA (Prod e1 e2)
	HI1:evalEA (expA2ea e1) ​=​ evalExpA e1
	HI2:evalEA (expA2ea e2) ​=​ evalExpA e2

Demostración
- Caso base 
	evalEA (expA2ea (Cte n))
	= expA2ea.1
	evalEA (Const n)
	= evalEA.1
	n
	=evalExpA.1
	evalExpA (Cte n)

- Caso ind1
	evalEA (expA2ea (Suma e1 e2)) ​
	=expA2ea.2
	evalEA (BOp Sum (expA2ea e1) (expA2ea e2) )
	=evalEA.2 
	evalBOp Sum (evalEA (expA2ea e1)) (evalEA (expA2ea e2))
	=HI1
	evalBOp Sum (evalExpA e1) (evalEA (expA2ea e2))
	=HI2
	evalBOp Sum (evalExpA e1) (evalExpA e2)
	=evalBOp.1
	(+) (evalExpA e1) (evalExpA e2)
	=sección operadores
	evalExpA e1 + evalExpA e2
	=evalExpA.2
	evalExpA (Suma e1 e2)
	
- Caso ind2 
	Similar a caso ind1

-}

--Ej 2
--Ej 2
data Arbol a b = Hoja b | Nodo a (Arbol a b) (Arbol a b) deriving Show

arbol = Nodo 4 (Nodo 3 (Hoja 1) (Hoja 2)) (Nodo 5 (Hoja 6) (Hoja 7))

cantidadDeHojas::Arbol a b ->Int
cantidadDeHojas (Hoja b) = 1
cantidadDeHojas (Nodo a t1 t2) = cantidadDeHojas t1 + cantidadDeHojas t2

cantidadDeNodos::Arbol a b->Int
cantidadDeNodos (Hoja b) = 0
cantidadDeNodos (Nodo a t1 t2) = 1 + cantidadDeNodos t1 + cantidadDeNodos t2

cantidadDeConstructores::Arbol a b->Int
cantidadDeConstructores (Hoja b) = 1
cantidadDeConstructores (Nodo a t1 t2) = 1 + cantidadDeConstructores t1 + cantidadDeConstructores t2

ea = BOp Sum (Const 1) (Const 2)

ea2Arbol::EA->Arbol BinOp Int
ea2Arbol (Const n) = Hoja n
ea2Arbol (BOp b e1 e2) = Nodo b (ea2Arbol e1) (ea2Arbol e2)

{-
Demostraciones
para todo ​t​​ :: Arbol a b  
	cantidadDeHojas ​t​ + cantidadDeNodos ​t​=​ cantidadDeConstructores ​t

Por ppio de inducción en la estructura de t
- Caso base t = Hoja e
	cantidadDeHojas ​(Hoja e)​ + cantidadDeNodos ​(Hoja e) =​ cantidadDeConstructores ​(Hoja e)
- Caso ind t = Nodo x t1 t2
	TI: cantidadDeHojas ​(Nodo x t1 t2)​ + cantidadDeNodos ​(Nodo x t1 t2) 
		=​ cantidadDeConstructores ​(Nodo x t1 t2)
	HI1: cantidadDeHojas t1  + cantidadDeNodos ​t1 =​ cantidadDeConstructores ​t1
	HI1: cantidadDeHojas t2  + cantidadDeNodos ​t2 =​ cantidadDeConstructores ​t2
	
Demostración
- Caso base
	cantidadDeHojas ​(Hoja e)​ + cantidadDeNodos ​(Hoja e)
	= cantidadDeHojas.1
	1 + cantidadDeNodos ​(Hoja e)
	= cantidadDeNodos.1
	1 + 0
	= aritmetica
	1
	
	cantidadDeConstructores ​(Hoja e)
	= cantidadDeConstructores.1
	1

- Caso ind
	cantidadDeHojas ​(Nodo x t1 t2)​ + cantidadDeNodos ​(Nodo x t1 t2)
	= cantidadDeHojas.2
	cantidadDeHojas t1 + cantidadDeHojas t2 + cantidadDeNodos ​(Nodo x t1 t2)
	= cantidadDeNodos.2
	cantidadDeHojas t1 + cantidadDeHojas t2 + 1 + cantidadDeNodos t1 + cantidadDeNodos t2
	= conmutativa
	cantidadDeHojas t1 + cantidadDeNodos t1 + 1 + cantidadDeHojas t2 + cantidadDeNodos t2
	= HI1
	cantidadDeConstructores ​t1 + 1 + cantidadDeHojas t2 + cantidadDeNodos t2
	= HI2
	cantidadDeConstructores ​t1 + 1 + cantidadDeConstructores ​t2
	= conmutativa
	1 + cantidadDeConstructores ​t1 + cantidadDeConstructores ​t2
	=cantidadDeConstructores.2
	cantidadDeConstructores ​(Nodo x t1 t2)

-}

--Ej 3
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show
sumarT::Tree Int->Int
sumarT EmptyT = 0
sumarT (NodeT e t1 t2) = e + sumarT t1 + sumarT t2

sizeT::Tree a->Int 
sizeT EmptyT = 0
sizeT (NodeT e t1 t2) = 1 + sizeT t1 + sizeT t2

anyT::(a->Bool)->Tree a->Bool
anyT p EmptyT = False
anyT p (NodeT e t1 t2) = (p e || anyT p t1) || anyT p t2

countT::(a->Bool)->Tree a->Int
countT p EmptyT = 0
countT p (NodeT e t1 t2) = unoSiCumpleT p e + countT p t1 + countT p t2

unoSiCumpleT::(a->Bool)->a->Int
unoSiCumpleT p e = if p e then 1 else 0

countLeaves::Tree a->Int
countLeaves EmptyT = 0
countLeaves (NodeT e t1 t2) = countLeaves t1 + countLeaves t2

heightT::Tree a->Int
heightT EmptyT = 0
heightT (NodeT e t1 t2) = 1 + heightT t1 + heightT t2

inOrder::Tree a->[a]
inOrder EmptyT = []
inOrder (NodeT e t1 t2) = inOrder t1 ++ [e] ++ inOrder t2

listPerLevel::Tree a->[[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT e t1 t2) = [e] : juntarT (listPerLevel t1) (listPerLevel t2)

juntarT::[[a]]->[[a]]->[[a]]
juntarT [] [] = []
juntarT (xs:xss) [] = xs : xss
juntarT [] (ys:yss) = ys : yss
juntarT (xs:xss) (ys:yss) = (xs ++ ys) : (juntarT xss yss)

mirrorT::Tree a->Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT e t1 t2) = NodeT e (mirrorT t2) (mirrorT t1)

levelN::Int->Tree a->[a]
levelN n EmptyT = []
levelN n (NodeT e t1 t2) = if n == 0 then [e] else levelN (n-1) t1 ++ levelN (n-1) t2

ramaMasLarga::Tree a->[a]
ramaMasLarga EmptyT = []
--ramaMasLarga (NodeT e t1 t2) = e: listaMax (ramaMasLarga t1) (ramaMasLarga t1)
ramaMasLarga (NodeT e t1 t2) = if heightT t1 > heightT t2 
			then x: ramaMasLarga t1 else x:  ramaMasLarga t2

listaMax::[a]->[a]->[a]
listaMax xs ys = if length xs > length ys then xs else ys

todosLosCaminos::Tree a->[[a]]
todosLosCaminos EmptyT  = [[]]
todosLosCaminos (NodeT e t1 t2) = map (agregarEn e) (todosLosCaminos t1 ++ todosLosCaminos t2)

agregarEn :: a -> [a] -> [a]
agregarEn e xs = e : xs

{-
Demostraciones
i) heightT ​=​ length . ramaMasLarga
Por ppio de extensionalidad sea t un Tree a cualquiera
heightT t ​=​ (length . ramaMasLarga) t
Por def (.)
heightT t ​=​ length (ramaMasLarga t)

Por ppio de inducción en la estructura de t
- Caso base t = EmptyT
	heightT EmptyT ​=​ length (ramaMasLarga EmptyT)
- Caso ind t = NodeT e t1 t2
	TI: heightT (NodeT e t1 t2) ​=​ length (ramaMasLarga (NodeT e t1 t2))
	HI1: heightT t1 ​=​ length (ramaMasLarga t1)
	HI1: heightT t2 ​=​ length (ramaMasLarga t2)

Demostración
- Caso base
	heightT EmptyT ​
	=​ heightT.1 
	0
	=length.1
	length []
	= ramaMasLarga.1
	length (ramaMasLarga EmptyT)
	
- Caso ind
	length (ramaMasLarga (NodeT e t1 t2))
	=ramaMasLarga.2
	length(e: listaMax (ramaMasLarga t1) (ramaMasLarga t2))
	= length.2 
	1 + length (listaMax (ramaMasLarga t1) (ramaMasLarga t2)	)
	= def listaMax
	1 + length (if length (ramaMasLarga t1) > length (ramaMasLarga t2) 
		then (ramaMasLarga t1) else (ramaMasLarga t2))
	
	Caso (ramaMasLarga t1) > length (ramaMasLarga t2)  = True
		1 + length (ramaMasLarga t1)
		
	Caso (ramaMasLarga t1) > length (ramaMasLarga t2)  = False
		1 + length (ramaMasLarga t2)	
		
	
HI1: heightT t1 ​=​ length (ramaMasLarga t1)
HI1: heightT t2 ​=​ length (ramaMasLarga t2)
heightT (NodeT e t1 t2) ​=​ length (ramaMasLarga (NodeT e t1 t2))
		
	heightT (NodeT e t1 t2)
	= heightT.2
	max (heightT t1) (heightT t2)
	= HI1 e HI2
	max (length (ramaMasLarga t1)) (length (ramaMasLarga t2))
		
	
ii) reverse . inOrder ​=​ inOrder . mirrorT
Por ppio de extensionalidad sea t un Tree a cualquiera
reverse . inOrder ) t=​ inOrder . mirrorT) t
Por de (.)
reverse (inOrder t)=​ inOrder (mirrorT t) 

Porp inducción en la estructura de t

- Caso base t = EmptyT
	reverse (inOrder EmptyT)=​ inOrder (mirrorT EmptyT) 
- Caso ind t = (NodeT x t1 t2)
	TI: reverse (inOrder (NodeT x t1 t2))=​ inOrder (mirrorT (NodeT x t1 t2))
	HI1: reverse (inOrder t1)=​ inOrder (mirrorT t1)
	HI2: reverse (inOrder t2)=​ inOrder (mirrorT t2)

Demostración
- Caso base

- Caso ind
	reverse (inOrder (NodeT x t1 t2))
	=inOrder.2
	reverse (inOrder t1 ++ [x] ++ inOrder t2)
	=LEMA
	reverse (inOrder t2) ++ [x] ++ reverse (inOrder t1)
	=HI1
	inOrder (mirrorT t2) ++ [x] ++ reverse (inOrder t1)
	=HI2
	inOrder (mirrorT t2) ++ [x] ++ inOrder (mirrorT t1)
	
	
	inOrder (mirrorT (NodeT x t1 t2))
	=mirrorT.2
	inOrder (NodeT x (mirrorT t2) (mirrorT t1) )
	=inOrder.2
	inOrder (mirrorT t2) ++ [x] ++ inOrder (mirrorT t1)
	
	LEMA
	reverse (xs ++ [x] ++ ys) = reverse ys ++ [x] ++ reverse xs
	
	reverse (xs ++ [x] ++ ys)
	=asociatividad
	reverse (xs ++ ([x] ++ ys))
	= prop practica 8 Sección I ej 2.h reverse (​xs​ ++ ​ys​) ​=​ reverse ​ys​ ++ reverse ​xs
	reverse ([x] ++ ys) ++ reverse ​xs
	=misma prop que antes
	reverse ys ++ reverse [x] ++ reverse xs
-}


data AppList a = Single a | Append (AppList a) (AppList a) deriving Show
--Ej 1
lenAL::AppList a->Int
lenAL (Single e) = 1 
lenAL (Append a1 a2) = lenAL a1 + lenAL a2

consAL::a->AppList a->AppList a
consAL x (Single e) = Append (Single x) (Single e)
consAL x (Append a1 a2) = Append (consAL x a1) a2
--consAL x a2 = Append (Single x) a2

headAL::AppList a->a
headAL (Single e) = e
headAL (Append a1 a2) = headAL a1

tailAL::AppList a->AppList a
tailAL (Single x) = error ""
tailAL (Append (Single e) a2) = a2
tailAL (Append a1 a2) = Append (tailAL a1) a2

snocAL::a->AppList a->AppList a
snocAL x (Single e) = Append (Single e) (Single x)
snocAL x (Append a1 a2) = Append a1 (snocAL x a2)
--consAL a1 x = Append a1 (Single x)

lastAL::AppList a->a
lastAL (Single e) = e
lastAL (Append a1 a2) = lastAL a2

initAL::AppList a->AppList a
initAL (Append a1 (Single e)) = a1
initAL (Append a1 a2) = Append a1 (initAL a2)

reverseAL::AppList a->AppList a
reverseAL (Single e) = (Single e)
reverseAL (Append a1 a2) = Append (reverseAL a2) (reverseAL a1)

elemAL::Eq a=>a->AppList a->Bool
elemAL x (Single e) = x == e
elemAL x (Append a1 a2) = (elemAL x a1) || (elemAL x a2)

appendAL::AppList a->AppList a->AppList a
appendAL (Single e) a' = Append (Single e) a'
appendAL (Append a1 a2) a'= Append (appendAL a1 a') a2
--appendAL a1 a2 = Append a1 a2

appListToList::AppList a->[a]
appListToList (Single e) = [e]
appListToList (Append a1 a2) = appListToList a1 ++ appListToList a2

{-
Demostraciones

i)para todo ​xs​​ :: AppList a​. para todo ​ys​​ :: AppList a​.    lenAL (appendAL ​xs ​​ys​) ​=​ lenAL ​xs​ + lenAL ​ys

Por ppio de inducción en la estuctura de xs
- Caso base xs = Single e
	lenAL (appendAL ​(Single e) ​​ys​) ​=​ lenAL ​(Single e)​ + lenAL ​ys
- Caso ind xs = (Append a1 a2)
	TI: lenAL (appendAL ​(Append a1 a2) ​​ys​) ​=​ lenAL ​(Append a1 a2)​ + lenAL ​ys
	HI1: lenAL (appendAL ​a1 ​​ys​) ​=​ lenAL a1 + lenAL ​ys
	HI2: lenAL (appendAL ​a2 ​​ys​) ​=​ lenAL a2 + lenAL ​ys


Demostración
- Caso base
	lenAL (appendAL ​(Single e) ​​ys​)
	=appendAL.1
	lenAL (Append (Single e) ​​ys)
	=lenAL.2
	lenAL (Single e) + lenAL ys
	
- Caso ind
	lenAL (appendAL ​(Append a1 a2) ​​ys​)
	=appendAL.2
	lenAL (Append (appendAL a1 ys) a2)
	= lenAL.2
	lenAL (appendAL a1 ys) + lenAL a2
	= HI1
	lenAL a1 + lenAL ​ys + lenAL a2
	= conmutativa
	lenAL a1 + lenAL a2 + lenAL ys
	= lenAL.2
	lenAL ​(Append a1 a2)​ + lenAL ​ys

ii) reverseAL . reverseAL ​=​ id
Por ppio de extensionalidad sea un a1 un AppList a cualquiera 
(reverseAL . reverseAL) a1 ​=​ id a1
Por def (.)
reverseAL (reverseAL a1) ​=​ id a1

Por ppio de inducción en la estructura de a1
- Caso base xs = Single e
	reverseAL (reverseAL (Single e)) ​=​ id (Single e)
- Caso ind xs = (Append a1 a2)
	TI: reverseAL (reverseAL (Append a1 a2)) ​=​ id (Append a1 a2)
	HI1: reverseAL (reverseAL a1) ​=​ id a1
	HI2: reverseAL (reverseAL a2) ​=​ id a2

Demostración

- Caso base 
	reverseAL (reverseAL (Single e))
	=reverseAL.1
	reverseAL (Single e)
	=reverseAL.1
	(Single e)
	= def id
	id (Single e)
	
- Caso ind
	reverseAL (reverseAL (Append a1 a2))
	 =reverseAL.2
	 reverseAL (Append (reverseAL a2) (reverseAL a1))
	 =reverseAL.2
	 Append (reverse (reverseAL a1)) (reverse (reverseAL a2))
	 = HI1
	 Append (id a1) (reverse (reverseAL a2))
	 = HI2
	 Append (id a1) (id a2)
	 =def id dos veces
	 Append a1 a2
	 = def id
	 id (Append a1 a2)

-}

--Ej 5
data QuadTree a = LeafQ a | NodeQ (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a) deriving Show
data Color = RGB Int Int Int
type Image = QuadTree Color

heightQT::QuadTree a->Int
heightQT (LeafQ e) = 1
heightQT (NodeQ q1 q2 q3 q4) = maxCuatro (heightQT q1) (heightQT q2) (heightQT q3) (heightQT q4)

maxCuatro::Int->Int->Int->Int->Int
maxCuatro m n o p = max m (max n (max o p))

countLeavesQT::QuadTree a->Int 
countLeavesQT (LeafQ e) = 1
countLeavesQT (NodeQ q1 q2 q3 q4) = countLeavesQT q1 + countLeavesQT q2 + countLeavesQT q3 + countLeavesQT q4

sizeQT::QuadTree a->Int 
sizeQT (LeafQ e) = 1
sizeQT (NodeQ q1 q2 q3 q4) = 1 + sizeQT q1 + sizeQT q2 + sizeQT q3 + sizeQT q4

compress::Eq a => QuadTree a->QuadTree a
compress (LeafQ e) = (LeafQ e)
compress (NodeQ q1 q2 q3 q4) = compressNodes (compress q1) (compress q2) (compress q3) (compress q4)

compressNodes::Eq a => QuadTree a->QuadTree a->QuadTree a->QuadTree a->QuadTree a
compressNodes (LeafQ m) (LeafQ n) (LeafQ o) (LeafQ p) = if (m == n && n == o) && o == p then LeafQ m else NodeQ (LeafQ m) (LeafQ n) (LeafQ o) (LeafQ p)

uncompress::QuadTree a->QuadTree a
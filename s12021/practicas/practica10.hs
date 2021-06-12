--Ejercicio 1) Dada la siguiente representación de un lenguaje de expresiones icas con variables:
data NExp = Var Variable | NCte Int | NBOp NBinOp NExp NExp
data NBinOp = Add | Sub | Mul | Div | Mod | Pow
type Variable = String

{-
y el TAD Memoria cuya interfaz es la siguiente:
	enBlanco :: Memoria, que describe una memoria vacía.
	cuantoVale :: Variable -> Memoria -> Maybe Int, que describe el número asociado a la variable dada en la memoria dada.
	recordar :: Variable -> Int -> Memoria -> Memoria, que la memoria resultante de asociar el número dado a la variable dada en la memoria dada.
	variables :: Memoria -> [Variable], que describe las variables que la memoria recuerda.
-}


-- a. implementar las siguientes funciones:

evalNExp :: NExp -> Memoria -> Int
evalNExp (Var v) mem = 
	case cuantoVale v m
	Nothing -> error("no existe la variable en la memoria")
	Just n -> n
evalNExp (NCte n) mem = n
evalNExp (NBOp nOp el e2) mem = evalNBop nOp (evalNExp e1 mem) (evalNExp e2 mem)

evalNBop::NBinOp -> int -> int
evalNBop Add = (+)
evalNBop Sub = (-)
evalNBop Mul = (*)
evalNBop Div = div
evalNBop Mod = mod
evalNBop Pow = (^)


cfNExp :: NExp -> NExp --, que describe una expresión con el mismo significado que la dada, pero simplificada y reemplazando las subexpresiones que no dependan de la memoria por su expresión más sencilla. La resolución debe ser exclusivamente simbólica
cfNExp (Var v) = Var v
cfNExp (NCte n) = NCte n
cfNExp (NBOp nOp el e2)  = cfNBinop nOp (cfNExp e1) (cfNExp e2)

cfNBinop :: NBinOp->NExp->NExp->NExp
cfNBinop Add ne1 ne2 = simpAddNExp ne1 ne2
cfNBinop Sub ne1 ne2 = simpSubNExp ne1 ne2
cfNBinop Mul ne1 ne2 = simpMulNExp ne1 ne2
cfNBinop Div ne1 ne2 = simpDivNExp ne1 ne2
cfNBinop Mod ne1 ne2 = simpModNExp ne1 ne2
cfNBinop Pow ne1 ne2 = simpPowNExp ne1 ne2


simpAddNExp :: NExp->NExp->NExp
simpAddNExp (NCte 0) ne2 = ne2
simpAddNExp ne1 (NCte 0) = ne1
simpAddNExp ne1 ne2 = NBOp Add ne1 ne2

{-
.b demostrar la siguiente propiedad:
	evalNExp . cfNExp = evalNExp
	
Por ppio de extensionalidad sea e un NExp cualquiera
	(evalNExp . cfNExp) e = evalNExp e
	
Por def (.)
	evalNExp (cfNExp e) = evalNExp e

Por ppio de extensionalidad sea m un Memoria cualquiera
	evalNExp (cfNExp e) m = evalNExp e m
	
Por ppio de inducción estructural en e
- Caso base e = Var v
	evalNExp (cfNExp (Var v)) m = evalNExp (Var v) m
- Caso base e = NCte n
	evalNExp (cfNExp (NCte n)) m = evalNExp (NCte n) m
- Caso ind e = NCte n
	TI: evalNExp (cfNExp (NBOp nOp el e2)) m = evalNExp (NBOp nOp el e2) m
	HI1: evalNExp (cfNExp el) m = evalNExp e1 m
	HI2: evalNExp (cfNExp e2) m = evalNExp e2 m

-Demostración
Caso base
	evalNExp (cfNExp (Var v)) m 
	= cfNExp.1
	evalNExp (Var v) m
	
Caso base
	evalNExp (cfNExp (NCte n)) m 
	= cfNExp.2
	evalNExp (NCte n) m
	
Caso ind
	evalNExp (cfNExp (NBOp nOp el e2)) m 
	= cfNExp.3
	evalNExp (cfNBinop nOp (cfNExp e1) (cfNExp e2)) m
	= LEMA evalNExp-cfNBinop
	evalNBop nOp (evalNExp (cfNExp el) m) (evalNExp (cfNExp e2) m)
	
	evalNExp (NBOp nOp el e2) m
	= evalNExp.3
	evalNBop nOp (evalNExp e1  m) (evalNExp e2 m)
	= HI1 HI2
	evalNBop nOp (evalNExp (cfNExp el) m) (evalNExp (cfNExp e2) m)
	
	LEMA evalNExp-cfNBinop 
		evalNExp (cfNBinop nOp e1 e2) m = evalNBop nOp (evalNExp e1 m) (evalNExp e2 m)
		
		Caso nOp = Add
			evalNBop Add (evalNExp e1 m) (evalNExp e2 m)
			= evalNBop.1
			(+) (evalNExp e1 m) (evalNExp e2 m)
			
			evalNExp (cfNBinop Add e1 e2) m 
			= cfNBinop.1
			evalNExp (simpAddNExp e1 e2) m
			
			- Caso e1 = NCte 0 y e2 distinto a NCte 0
				evalNExp (simpAddNExp (NCte 0) e2) m
				= simpAddNExp.1
				evalNExp e2 m
					
				(+) (evalNExp (NCte 0) m) (evalNExp e2 m)
				= evalNExp.2
				(+) 0 (evalNExp e2 m)
				= aritmetica
				evalNExp e2 m
								
			- Caso e2 = NCte 0 y e1 distinto a NCte 0
				evalNExp (simpAddNExp e1 (NCte 0)) m
				= simpAddNExp.2
				evalNExp e1 m
					
				(+) (evalNExp e1 m) (evalNExp (NCte 0) m)
				= evalNExp.2
				(+) (evalNExp e1 m)  0 
				= aritmetica
				evalNExp e1 m
			
			
			- Caso e1 y e2 distinto a NCte 0
				evalNExp (simpAddNExp e1 e2) m
				= simpAddNExp.3
				evalNExp (NBOp Add ne1 ne2) m
				= evalBExp.3
				evalNBop Add (evalNExp e1 m) (evalNExp e2 m)
				= evalNBop.1
				(+) (evalNExp e1 m) (evalNExp e2 m)

		Misma idea para los demas casos...

-}


--Ejercicio 2) Dada la siguiente representación de un lenguaje de expresiones booleanas:
data BExp = BCte Bool | Not BExp | And BExp BExp | Or BExp BExp | ROp RelOp NExp NExp
data RelOp = Eq | NEq | -- Equal y NotEqual
			 Gt | GEq | -- Greater y GreaterOrEqual
			 Lt | LEq -- Lower y LowerOrEqual
			 
--a. implementar las siguientes funciones:
--i. describe el booleano que resulta de evaluar la expresión dada a partir de la memoria dada.
evalBExp :: BExp -> Memoria -> Bool
evalBExp (BCte b) m = b
evalBExp (Not be) m = not (evalBExp be m)
evalBExp (And b1 b2) m = (&&) (evalBExp b1 m) (evalBExp b2 m)
evalBExp (Or b1 b2) m =  (||) (evalBExp b1 m) (evalBExp b2 m)
evalBExp (ROp rOp n1 n2) m = evalROp rOp (evalNExp n1 m) (evalNExp n2 m)

evalROp:: RelOp -> Int -> Int -> Bool
evalROp Eq n m = n == m
evalROp NEq n m = n !=m
evalROp Gt n m = n > m
evalROp GEq n m = n >= m
evalROp Lt n m = n < m
evalROp LEq n m = n <= m

--ii.que describe una expresión con el mismo significado que la dada, pero reemplazando las subexpresiones que no dependan de la memoria por su expresión más sencilla. La resolución debe ser exclusivamente simbólica.
cfBExp::BExp->BExp
cfBExp (BCte b) = (BCte b)
cfBExp (Not be) = cfNot (cfBExp be)
cfBExp (And be1 be2) = cfAnd (cfBExp be1) (cfBExp be2)
cfBExp (Or be1 be2) = cfOr (cfBExp be1) (cfBExp be2)
cfBExp (ROp r ne1 ne2) = cfRelOp r (cfNExp ne1) (cfNExp ne2) 

cfNot::BExp->BExp
cfNot (BExp b) = BExp (not b)

cfAnd::BExp->BExp->BExp
cfAnd (BCte b) be2 = if b then be2 else BCte False
cfAnd be1 be2 = And be1 be2

cfOr::BExp->BExp->BExp
cfOr (BCte b) be2 = if b then BCte True else be2
cfOr be1 be2 = Or be1 be2

cfRelOp::RelOp->NExp->NExp->BExp
cfRelOp r (NCte n) (NCte m) = BCte (evalRelOp r n m) 
cfRelOp r ne1 ne2           = ROp r ne1 ne2

{-
demostrar la siguiente propiedad:

i. evalBExp . cfBExp = evalBExp

Por ppio de extensionalidad sea be una BExp cualquiera
	(evalBExp . cfBExp) be = evalBExp be
Por def (.)
	evalBExp (cfBExp be) = evalBExp be
Por ppio de extensionalidad sea m una Memoria cualquiera
	evalBExp (cfBExp be) m = evalBExp be m

Por ppio de inducción estructural en BExp
- Caso base1 be =(BCte b)
	evalBExp (cfBExp (BCte b)) m = evalBExp (BCte b) m
- Caso base2 be =(Not be)
	evalBExp (cfBExp (Not be) m = evalBExp (Not be) m
- Caso ind1 be =(And b1 b2)
	TI: evalBExp (cfBExp (And b1 b2) m = evalBExp (And b1 b2) m
	HI1: evalBExp (cfBExp b1) m = evalBExp b1 m
	HI2: evalBExp (cfBExp b2) m = evalBExp b2 m
- Caso ind2 be =(Or b1 b2)
	TI: evalBExp (cfBExp (Or b1 b2) m = evalBExp (Or b1 b2) m
	HI1: evalBExp (cfBExp b1) m = evalBExp b1 m
	HI2: evalBExp (cfBExp b2) m = evalBExp b2 m
- Caso base2 be = (ROp rOp n1 n2)
	evalBExp (cfBExp (ROp rOp n1 n2) m = evalBExp (ROp rOp n1 n2) m


Demostración

- Caso base1
	evalBExp (cfBExp (BCte b)) m 
	= cfBExp.1
	evalBExp (BCte b) m
	
- Caso base2
	evalBExp (Not be) m
	= evalBExp.2
	not (evalBExp be m)
	
	evalBExp (cfBExp (Not b)) m 
	= cfBExp.2
	evalBExp (cfNot (cfBExp b)) m
	= def cfNot
	evalBExp (BExp (not b)) m
	
- Caso ind1 be =(And b1 b2)
	evalBExp (And b1 b2) m
	=evalBExp.3
	(&&) (evalBExp b1 m) (evalBExp b2 m)
	= HI1 e HI2
	(&&) (evalBExp (cfBExp b1) m) (evalBExp (cfBExp b2) m)
		
	evalBExp (cfBExp (And b1 b2) m 
	= cfBExp.3
	evalBExp (cfAnd (cfBExp b1) (cfBExp b2)) m
		
		Caso b1 =(BCte b) y b2 distinto a (BCte b)
			(&&) (evalBExp (cfBExp (BCte b)) m) (evalBExp (cfBExp b2) m)
			= cfBExp.1
			(&&) (evalBExp (BCte b) m) (evalBExp (cfBExp b2) m)
			= evalBExp.2
			(&&) b (evalBExp (cfBExp b2) m)
			
			
			evalBExp (cfAnd (cfBExp (BCte b)) (cfBExp b2)) m
			= cfAnd.1
			evalBExp (if b then (cfBExp b2) else BCte False) m
			
				Caso b = True
					evalBExp (if True then (cfBExp b2) else BCte False) m
					= def if
					evalBExp (cfBExp b2) m
	
					(&&) True (evalBExp (cfBExp b2) m)
					= def and
					(evalBExp (cfBExp b2) m)
		
		Caso be1 y be2 distinto a BCte b
	
			evalBExp (cfAnd (cfBExp be1)) (cfBExp be2)) m
			=cfAnd.2
			evalBExp (And (cfBExp be1) (cfBExp be2)) m
			= evalBExp.3
			(&&) (evalBExp (cfBExp be1) m) (evalBExp (cfBExp be2) m)
	
- Caso ind2 be =(Or b1 b2)
	Misma idea que lo resuelto en And

- Caso base2 be = (ROp rOp n1 n2)
	evalBExp (cfBExp (ROp rOp n1 n2) m
	= cfBExp.5
	evalBExp (cfRelOp rOp (cfNExp n1) (cfNExp n2) ) m
	
	evalBExp (ROp rOp n1 n2) m
	= evalBExp.4
	evalROp rOp (evalNExp n1 m) (evalNExp n2 m)
	
		Caso rOp = Eq
			evalROp Eq (evalNExp n1 m) (evalNExp n2 m)
			= evalROp.1
			(evalNExp n1 m) == (evalNExp n2 m)
			
			evalBExp (cfRelOp Eq (cfNExp n1) (cfNExp n2) ) m
			
			
-}
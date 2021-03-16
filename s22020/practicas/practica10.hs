data NExp = Var Variable | NCte Int | NBOp NBinOp NExp NExp
data NBinOp = Add | Sub | Mul | Div | Mod | Pow
type Variable = String

evalNExp::NExp->Memoria->Int
evalNExp (Var v) m = case cuantoVale v m
	Nothing -> error "No existe la variable solicitada"
	Just n -> n
evalNExp (NCte n) m = n
evalNExp (NBOp nBop ne1 ne2) m = evalNBinop nBop (evalNExp ne1 m) (evalNExp ne2 m)  

evalNBinop::NBinOp->Int -> Int ->Int
evalNBinop Add = (+)
evalNBinop Sub = (-)
evalNBinop Mul = (*)
evalNBinop Div = div
evalNBinop Mod = mod
evalNBinop Pow = (^)

cfNExp::NExp->NExp
cfNExp (Var v) = Var v
cfNExp (NCte n) = NCte n
cfNExp (NBOp nBop ne1 ne2) = cfNBinop nBop (cfNExp ne1) (cfNExp ne1)

cfNBinop:NBinOp->NExp->NExp->NExp
cfNBinop nBop (NCte n) (NCte m) = NCte (evalNBinop nBop n m)
cfNBinop nBop ne1 ne2 = NBOp nBop ne1 ne2

{-
Demostraciones
¿evalNExp . cfNExp ​=​ evalNExp?

Por ppio de extensionalidad sea ne un NExp cualquiera y m una memoria cualquiera
¿evalNExp . cfNExp) ne m ​=​ evalNExp ne m?
Popr def (.)
¿evalNExp (cfNExp ne) m ​=​ evalNExp ne m?

Por ppio de inducción en la estructura de ne
- Caso base 1 ne = Var v
	¿evalNExp (cfNExp (Var v) m) ​=​ evalNExp (Var v) m?
- Caso base 2 ne = NCte n
	¿evalNExp (cfNExp (NCte n) m) ​=​ evalNExp (NCte n) m?
- Caso ind ne = NBOp nBop ne1 ne2
	TI: ¿evalNExp (cfNExp (NBOp nBop ne1 ne2)) m ​=​ evalNExp (NBOp nBop ne1 ne2) m?
	HI1: ¡evalNExp (cfNExp ne1) m ​=​ evalNExp ne1 m!
	HI2: ¡evalNExp (cfNExp ne2) ​m =​ evalNExp ne2 m!

Demostración
- Caso base 1
	evalNExp (cfNExp (Var v)) m​
	=​ cfNExp.1
	evalNExp (Var v) m
- Caso base 2
	evalNExp (cfNExp (NCte n)) ​m
	=​ cfNExp.2
	evalNExp (NCte n) m
	
- Caso ind 
	evalNExp (cfNExp (NBOp nBop ne1 ne2)) m
	​=​ cfNExp.3
	evalNExp (cfNBinop nBop (cfNExp ne1) (cfNExp ne2)) m
	= evalNExp.3
	evalNBinop nBop (evalNExp (cfNExp ne1) m) (evalNExp (cfNExp ne2) m) 
	=HI1 e HI2
	evalNBinop nBop (evalNExp ne1 m) (evalNExp ne2 m) 
	= evalNExp.3
	evalNExp (NBOp nBop ne1 ne2) m
	
-}

--Ej 2)

data BExp = BCte Bool | Not BExp | And BExp BExp | Or BExp BExp | ROp RelOp NExp NExp
data RelOp = Eq | NEq | Gt | GEq | Lt | LEq
--equal, not equal, greater, greater or equal, lower than, lower or equal

evalBExp::BExp->Memoria->Bool
evalBExp (BCte bool) m = bool
evalBExp (Not be) m = not (evalBExp be)
evalBExp (And be1 be2) m = (evalBExp be1) && (evalBExp be2)
evalBExp (Or be1 be2) m = (evalBExp be1) || (evalBExp be2)
evalBExp (ROp r ne1 ne2) m = evalRelOp r (evalNExp ne1 m) (evalNExp ne2 m) 

evalRelOp::RelOp->Int->Int->Bool
evalRelOp Eq  = (==)
evalRelOp NEq = (/=)
evalRelOp Gt  = (>)
evalRelOp GEq = (>=)
evalRelOp Lt  = (<)
evalRelOp LEq = (<=)

cfBExp::BExp->BExp
cfBExp (BCte bool) = bool
cfBExp (Not be) = cfNot (cfBExp be)
cfBExp (And be1 be2) = cfAnd (cfBExp be1) (cfBExp be2)
cfBExp (Or be1 be2) = cfOr(cfBExp be1) (cfBExp be2)
cfBExp (ROp r ne1 ne2) = cfRelOp r (cfNExp ne1) (cfNExp ne1) 

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


--Ej 3

data Programa = Prog Bloque
type Bloque = [Comando]
data Comando = Assign Nombre NExp | If BExp Bloque Bloque | While BExp Bloque

evalProg::Programa->Memoria->Memoria
evalProg (Prog bl) m = evalBlq bl m

evalBlq::Bloque->Memoria->Memoria
evalBlq [] m  = m
evalBlq (c:cs) m = let mem' = evalCom c m in evalBlq cs mem'

evalCom::Comando->Memoria->Memoria
evalCom (Assign name ne) m = recordar name (evalNExp ne m) m 
evalCom (If be bq1 bq2)  m = if evalBExp be m then evalBlq bq1 m else evalBlq bq2 m
evalCom (While be bq)    m = evalCom (If be (bq++[While be bq]) []) m

optimizeCf::Programa->Programa
optimizeCf (Prog bq) = optimizeCfBq bq

optimizeCfBq::Bloque->
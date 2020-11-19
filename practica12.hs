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

--cantDeSumaCeros::ExpA->Int
--cantDeSumaCeros = foldExpA ? ? ?

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

ea2ExpA'::EA->ExpA​
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


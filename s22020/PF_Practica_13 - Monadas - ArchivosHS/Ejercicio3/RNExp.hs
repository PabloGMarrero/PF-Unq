module RNExp where

import Monadas      
import ReaderMonad  -- La mónada Reader 
import Mem          -- La clase de las memorias (¿es un TAD? ;))
import ListMem      -- La memoria hecha con listas

data NExp = Var Variable 
          | NCte Int
          | UOp UnOp NExp
          | BOp BinOp NExp NExp

data UnOp = Neg | Sqr
data BinOp = Add | Mul | Div | Mod

ejNE = BOp Add (Var "x") (NCte 1)

ejM1, ejM2 :: ListMem 
ejM1 = recordar "x" 16 enBlanco
ejM2 = recordar "x" 41 enBlanco

-- Esta es otra forma de especializar una función sobrecargada para
-- usar un tipo específico
-- (la declaración del tipo ES FUNDAMENTAL para que esto sirva)
miEvalNE :: NExp -> Reader ListMem Int
miEvalNE e = evalNE e

-- Funciona porque miEvalNE NO es genérica 
-- (con evalNE da error, porque no puede saber qué mónada queremos para reader)
ej1 = runRM (miEvalNE ejNE) ejM1
ej2 = runRM (miEvalNE ejNE) ejM2

-- COMPLETAR para que evalNE sea una función total que implemente 
-- el significado de su argumento como una mónada reader
evalNE :: (Mem mem, ReaderMonad mem m) => NExp -> m Int
--evalNE (Var variable)    = ask (cuantoVale variable)                           
evalNE (NCte n)          = return n
evalNE (UOp uOp ne)      = evalUO uOp (evalNE ne)   --esto no tipa ya que evalNE ne devuelve un m Int pero evalUO espra un INT
--evalNE (BOp bOp ne1 ne2) = evalNE (evalBO bOp ne1) </> evalNE (evalBO bOp ne2) --esto no tipa ya que evalNE ne devuelve un m Int pero evalUO espra un INT
evalNE _ = error "Dale papa metele. Implementame"


evalUO Neg x = return (-x)
evalUO Sqr x = return (x^2)

evalBO Add x y = return (x+y)
evalBO Mul x y = return (x*y)
evalBO Div x y = return (div x y)
evalBO Mod x y = return (mod x y)

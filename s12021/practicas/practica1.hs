-- Ejercicio 1: Denotar de 8 maneras el numero 4
{-
    2+2
    1+3
    doble 2
    (doble (doble 1))
    (doble (doble (-1))) * (-1)
    cuadruple 1
    \x -> 4
    \x -> 2 * 2 
-}

-- Ejercicio 2
doble x = x + x
{-
    


    doble (doble 2)
    = def doble con x <= 2
    doble (2 + 2)
    = aritmetica
    doble 4
    = def doble con x <= 4
    4 + 4
    = aritmetica
    8

    doble (doble 2)
    = def doble con x <= doble 2
    doble 2 + doble 2
    = def doble con x <= 2
    2 + 2 + doble 2
    = def doble con x <= 2
    2 + 2 + 2 + 2
    = aritmetica
    8  
-}

-- Ejercicio 3
cuadruple x = 4*x

{-
    cuadruple 2
    = def cuadruple con x <= 2
    4 * 2
    = aritmetica
    8


    cuadruple (cuadruple 2)
    = def cuadruple con x <- 2
    cuadruple (4 * 2)
    = aritmetica
    cuadruple 8
    = def cuadruple con x <- 8
    cuadruple (4 * 8)
    = aritmetica
    32

    Si, cuadruple (cuadruple 2) puede realizarse siguiendo la misma idea del ejercicio 2    
-}

-- Ejercicio 4
triple x = 3 * x
succ x = x + 1
sumarDos x = x + 2

-- Ejercicio 5

twice f = g
 where g x = f (f x)

{-
    Comprobar twice succ  = sumarDos

    twice succ 2 = sumarDos 2

    (twice succ) 2
    = def twice con f <- succ
    g 2 
    = donde g x <- succ (succ x) 
        def de g con x = 2
    succ (succ 2)
    = def succ x <- 2
    succ (2 + 1)
    = aritmetica
    succ 3
    = def succ x <- 3
    succ (3 + 1)
    = aritmetica
    4
    

    sumarDos 2 
    = def sumarDos x <- 2
    2 + 2
    = aritmetica
    4
-}

-- Ejercicio 6
{-
    Dar tres pares de expresiones equivalentes

    doble x = succ 
-}

-- Ejercicio 7
{-
    ((twice twice) doble) 3
    = def twice con f <- twice
    (g doble) 3
    = donde g x <- twice (twice x) 
        donde x <- doble
    (twice (twice doble)) 3
    = def twice con f <- twice doble
    g 3
    = donde g x <-  (twice doble) (( twice doble) x)
        donde x <- 3
    (twice doble) ((twice doble) 3)
    = def twice con f <- doble
    (twice doble) (g 3)
    = donde g x <- doble (doble x)
        donde x <- 3
    (twice doble) (doble (doble 3))
    = def doble
    (twice doble) (doble (2*3))
    = aritmetica
    (twice doble) (doble 6)
     = def doble
    (twice doble) (2*6)
    = aritmetica
    (twice doble) 12
    = def twice f <- doble
    g 12
    = donde g x <-  doble (doble x)
        donde x <- 12
    doble (doble 12)
    = def doble x <- 12
    doble (2*12)
    = aritmetica
    doble 24
    = def doble x <- 24
    2*24
    = aritmetica
    48
-}

-- Ejercicio 8: Definir equivalentes a tripke, succ, sumarDos, twice y twice twice

    triple = \x -> x*3
    succ = \x -> x+1
    sumarDos = \x -> x+2
    twice = \f -> \x -> f (f x)
    twice twice = \f -> \ x -> \

-- Ejercicio 
{-
    Reescribir las siguientes funciones

    a- f x = let (y,z) = (x,x) in y
        f x = id 
    b- f (x,y) = let z = x + y in g (z,y) where g (a,b) = a - b
        f (x, y) = fst x
    c- f p = case p of (x,y) -> x
        f p = fst
    d- f = \p -> let (x,y) = p in y
        f = snd
-}

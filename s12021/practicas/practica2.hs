--Ejercicio 1) Indicar los tipos de las siguientes definiciones:
{-

a)
first (x,y) = x

first  :: (a, b) -> a
(x, y) :: (a, b)
--------------------
first (x,y) :: a

b)
apply f = g
	where g x = f x
	
	
apply   :: (a -> b ) -> (a -> b )     g   :: a -> b         f   :: a -> b
f       ::  a ->  b                 x   :: a              x   :: a
--------------------------------    -----------------    ----------------
apply f :: a -> b                   g x ::  b             f x :: b


c)
twice f = g
	where g x = f (f x)

	
twice   :: (a -> a) -> (a-> a)      g   :: a  ->   a       f     :: a -> a        f   :: a -> a
f       ::  a ->  a                 x   ::   a            (f x)  :: a             x   :: a
--------------------------------    -----------------    -------------------     ----------------
twice f :: (a-> a)                  g x ::    a            f (f x)::  a           f x :: 


d)
doble x = x + x

doble   ::  Int -> Int      x    :: Int
x       ::  Int             x    :: Int
---------------------      ------------    
doble x ::   Int            x + x :: Int
 

e)
swap (x, y) = (y, x)

swap        ::  (a, b) -> (b, a)
(x, y)      ::  (a, b)
---------------------------------
swap (x, y) ::  (b, a)

f)
uflip f = g
	where g p = f (swap p)

uflip    :: ((b, a) -> c ) -> ((a, b) -> c)      g   :: (a, b) -> c        f        ::  (b, a) -> c      swap    ::  (a, b) -> (b, a)
f        :: (b, a) -> c                          p   :: (a, b)            (swap p)  ::  (b, a)            p      ::  (a, b)
-----------------------------------------	   -----------------------   ----------------------------   -------------------------- 
uflip f  :: (a, b) ->     c                      g p ::     c             f (swap p)::   c               (swap p)::  (b, a)
	
-}



--Ejercicio 2) Dadas las definiciones anteriores, indicar el tipo de las siguientes expresiones:

{-
a. apply first

apply       :: (a' -> b' ) -> (a' -> b' )
first       :: (a, b)   -> a                   (a, b) = a' ; b' = a
------------------------------------------
apply first ::

apply       :: ((a, b) -> a ) -> ((a, b) -> a)
first       :: (a, b)   -> a                   
------------------------------------------------
apply first :: (a, b) -> a)


b. first (swap, uflip)

first               ::                     (a', b')                             -> a'
(swap, uflip)       :: ((a, b) -> (b, a) , ((b, a) -> c ) -> ((a, b) -> c))                 a' = (a, b) -> (b, a)  ; b' = ((b, a) -> c ) -> ((a, b) -> c)
------------------------------------------------------------------------------
first (swap, uflip) :: 



first               ::     ((a, b) -> (b, a), ((b, a) -> c ) -> ((a, b) -> c))  -> (a, b) -> (b, a) 
(swap, uflip)       :: ((a, b) -> (b, a) , ((b, a) -> c ) -> ((a, b) -> c))  
------------------------------------------------------------------------------
first (swap, uflip) :: (a, b) -> (b, a)


c. twice doble

twice       ::   (a -> a) -> (a-> a)
doble       ::   Int -> Int                        a = Int
-------------------------------------
twice doble ::


twice       ::  (Int -> Int) -> (Int-> Int)
doble       ::   Int -> Int             
-------------------------------------
twice doble ::(Int-> Int)


d. twice twice

twice       ::        (a' -> a') -> (a'-> a')
twice       :: (a -> a) -> (a-> a)                    a' = (a -> a)
----------------------------------------
twice twice ::

twice       :: ((a -> a) -> (a -> a)) -> ((a -> a)-> (a -> a))
twice       :: (a -> a) -> (a-> a)                    
----------------------------------------
twice twice :: (a -> a)-> (a -> a)

e. twice uflip

twice       ::             (a' -> a')             -> (a' -> a') 
uflip       :: ((b, a) -> c ) -> ((a, b) -> c)                       a' = ((b, a) -> c ) ;  a'=  ((a, b) -> c) => a = b
-----------------------------------------------------------
twice uflip ::

twice       ::  (((a, a) -> c) -> (a, a) -> c))  -> ((a, a) -> c) -> (a, a) -> c)) 
uflip       :: ((a, a) -> c ) -> ((a, a) -> c)                                         a' = ((b, a) -> c ) ;  a'=  ((a, b) -> c) => a = b
-----------------------------------------------------------
twice uflip :: ((a, a) -> c) -> (a, a) -> c)) 


f. twice swap

twice      ::    (a' -> a')     -> (a' -> a')
swap       :: (a, b) -> (b, a)                       a' = (a, b) ; a' = (b, a)  ; a = b => a' = (a, a)
-----------------------------
twice swap ::


twice      ::((a, a) -> (a, a))  -> ((a, a)-> (a, a))
swap       :: (a, a) -> (a, a)                            a' = (a, b) ; a' = (b, a)  ; a = b => a' = (a, a)
----------------------------------------------------------
twice swap :: (a, a)-> (a, a)

g. uflip swap

uflip      :: ((b', a') -> c' ) -> ((a', b') -> c')  
swap       ::  (a, b) -> (b, a)                            b' = a ; a' = b  ; c' = (b, a)
-------------------------------------------------
uflip swap ::


uflip      :: ((a, b) -> (b, a) ) -> ((b, a) -> (b, a))  
swap       ::  (a, b) -> (b, a)                            b' = a ; a' = b  ; c' = (b, a)
-------------------------------------------------
uflip swap :: (b, a) -> (b, a)

h. (twice twice) swap

(twice twice)      :: (a'     ->     a')  -> (a' -> a')
swap               :: (a, b) -> (b, a)                             a' = (a, b) ; a' = (b, a) ; => a = b
------------------------------------------
(twice twice) swap ::


(twice twice)      :: ((a, a)->  (a, a))  -> ((a, a) -> (a, a))
swap               :: (a, a) -> (a, a)                             a' = (a, b) ; a' = (b, a) ; => a = b
------------------------------------------
(twice twice) swap :: ((a, a) -> (a, a))

-}

--Ejercicio 3) Dadas las siguientes definiciones y los siguientes tipos, asociar cada tipo con la función correspondiente.
{-
a. const x = g
 where g y = x
b. appDup f = g
 where g x = f (x, x)
c. appFork (f, g) = h
 where h x = (f x, g x)
d. appPar (f, g) = h
 where h (x, y) = (f x, g y)
e. appDist f = g
 where g (x, y) = (f x, f y)
f. flipp f = h
 where h x = k
        where k y = (f y) x
g. subst f = h
 where h g = k
  where k x = (f x) (g x)
  
 I. (a -> b, c -> d) -> ((a, c) -> (b, d))
II. ((a, a) -> b) -> (a -> b)
III. (a -> (b -> c)) -> (b -> (a -> c))
IV. (a -> b) -> ((a, a) -> (b, b))
V. (a -> b, a -> c) -> (a -> (b, c))
VI. (a -> (b -> c)) -> ((a -> b) -> (a -> c))
VII. a -> (b -> a)


 RTA
a. -> VII
b. -> II
c. -> V
d. -> I
e. -> IV
f. -> III
g. -> VI
-}


--Ejercicio 4) Para cada una de las siguientes expresiones decidir si poseen tipo. Si es así indicar cuál es.
{-
a. 1 && 2 == 2
b. 1 + if 3 < 5 then 3 else 5
c. let par = (True, 4)
	in (if first par then first par else second par)
d. (doble doble) 5
e. doble (doble 5)
f. twice first
g. (twice doble) doble
h. (twice twice) first
i. apply apply

a. No tiene tipo 
b. Tiene tipo Int
c. No tiene tipo
d. No tiene tipo
e. Tiene tipo Int
f. No tiene tipo 
g. No tiene tipo
h. Tiene tipo ( (a, b), c) ?
i. Tiene tipo (a->b) -> (a->b) 
 
-}

-- Ejercicio 5 sencillo, dar tipos. No es relevante

--Ejercicio 6) Para cada una de las siguientes expresiones, decir a cuál función del ejercicio 3 es equivalente. Ofrecer argumentos de por qué son equivalentes.

{-
a. \p -> let (f, g) = p
 in \x -> (f x, g x)
b. \f -> (\g -> (\x -> f x (g x))
c. \f -> (\x -> (\y -> (f y) x)
d. \f -> (\px -> let (x, y) = px
 in (f x, f y))
e. \x -> (\y -> x)
f. \pf -> let (f, g) = pf
 in \px -> let (x, y) = px
   in (f x, g y)
g. \f -> (\x -> f (x, x))

RTA
6a. -> 3 
6b. -> 3b 
6c. -> 3f
6d. -> 3d
6e. -> 3a 
6f. -> 3 
6g. -> 3b 

-}

--Ejercicio 7) Encontrar cuales de estas expresiones son equivalentes entre sí.
{-
a. appFork (id,id)          :: a -> (a, a)
b. \f -> appDup (appDist f) :: (a-> b) -> (a -> (b, b))
c. appDup id                :: b -> (a, a)
d. appDup appFork           :: (a-> b) -> (a -> (b, b))
e. flip (appDup const)      :: (a-> (b ->(b, b))
f. const (appDup id)        :: (a-> (b ->(b, b))


-}
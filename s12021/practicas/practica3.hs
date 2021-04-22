-- Ejercicio 1) Definir funciones

curry' :: ((a,b) -> c) -> a -> b -> c
curry' f = g
 where g x y = f (x, y)

curry'' :: ((a,b) -> c) -> a -> b -> c
curry'' f = \x -> \y -> f (x, y)


uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f = g 
 where g (x, y) = f x y

uncurry'' :: (a -> b -> c) -> (a,b) -> c
uncurry'' f = \(x, y) -> f x y


--Ejercicio 2) Reescribir las siguientes definiciones sin utilizar where, let o lambdas, y utilizando la menor cantidad de paréntesis posible.
-- Indicar el tipo de cada una de las funciones del ejercicio anterior, utilizando también la menor cantidad posible de paréntesis.


{-
a. apply f = g
 where g x = f x
b. twice f = g
 where g x = f (f x)
c. id = \x -> x
d. flip f = g
 where g x = h
       h y = (f y) x
e. uflip f = g
 where g p = f (swap p)
f. const = \x -> (\y -> x)
g. compose = \f -> (\g -> (\x -> f (g x)))
-}
apply:: (a -> b) -> a -> b
apply f x = f x

twice:: (a->a) -> (a->a)
twice f x = f (f x)

id :: a -> a
id x = x

flip:: (b -> a -> c) -> a -> b -> c
flip f x y = (f y) x

uflip:: ((a, b) -> c) -> (b, a-) -> c
uflip f p = f (swap p)

const:: a -> b -> a
const x y = x

compose:: ( b-> c) -> ( a-> b) -> a -> c
compose f g x = f (g x)

-- Ejercicio 4) En las expresiones que siguen, colocar los paréntesis que están implícitos, manteniendo el significado de cada una de las expresiones, y dar el tipo de cada una de ellas, suponiendo dadas las definiciones de los ejercicios anteriores.

{-
a. apply apply apply
b. twice doble 2
c. twice twice twice swap
d. flip twice 1 doble
-}

-- ((apply apply) apply)
-- ((twice doble) 2)
-- (((twice twice) twice) swap)
-- (((flip twice) 1) doble)


--Ejercicio 5) Reescribir las siguientes definiciones utilizando sólo lambdas (sin where ni let).

{-a. appDup f = g
 where g x = f (x, x)
b. appFork (f, g) = h
 where h x = (f x, g x)
c. appPar (f, g) = h
 where h (x, y) = (f x, g y)
d. appDist f = g
 where g (x, y) = (f x, f y)
e. subst f = h
 where h g = k
        where k x = (f x) (g x)
-}

appDup f = \x -> f (x, x)
appFork (f, g) = \x-> (f x, g x)
appPar (f, g) = \(x, y) -> (f x, g y)
appDist f = \(x, y) -> (f x, f y)
subst f = \g ->(\x -> (f x) (g x))

--Ejercicio 6) Indicar cuáles de las siguientes expresiones tienen tipo según el sistema de tipos de Hindley Milner. En el caso de que alguna sea incorrecta, ¿existirá una expresión que utilice las mismas partes, pero asociadas de forma diferente y que sí tenga significado? En el caso de que sí, escribir tal variante.

{-
a. compose (fst snd)
b. (uncurry curry snd)
c. (apply id) ((id apply) apply)
d. compose (compose doble doble)
e. (compose compose) doble doble
-}

-- a) tiene tipo
-- b) no tiene tipo, uncurry (curry snd)
-- c) tiene tipo
-- d) tiene tipo 
-- e) no tiene tipo, compose (compose doble doble)

-- Ejercicio 7) Dada la siguiente definición, indicar cómo podría reescribirse usando compose y id:

{-
many :: Int -> (a -> a) -> a -> a
many 0 f x = x
many n f x = f (many (n-1) f x)
-}

many :: Int -> (a -> a) -> a -> a
many 0 f = id
many n f = \x -> (f (many (n-1) f x))


-- Ejercicio 8) Quitar de los siguientes tipos la mayor cantidad de paréntesis posible, sin cambiar su significado. En cada caso, escribir en castellano cómo debería leerse el tipo obtenido de forma correcta, y cómo con la convención de leerla como si estuviera no-currificada.

{-Por ejemplo, para Int -> (Int -> Int) las respuestas serían: Int -> Int ->
Int, en castellano en forma correcta “es una función que toma un entero y
devuelve una función que toma otro entero y devuelve un entero”, y en castellano
usando la convención “es una función que toma dos enteros y devuelve un entero”.
a. (Int -> Int) -> (Int -> Int)
b. (a -> (b -> c)) -> (a -> b) -> c
c. (a -> b, c -> d) -> ((a, c) -> (b, d))
d. ((a, a) -> b) -> (a -> b)
e. (a -> (b -> c)) -> (b -> (a -> c))
f. (a -> b) -> ((a, a) -> (b, b))
g. (a -> b, a -> c) -> (a -> (b, c))
h. (a -> (b -> c)) -> ((a -> b) -> (a -> c))
i. a -> (b -> a)

a. (Int -> Int) -> Int -> Int
b. (a -> (b -> c)) -> (a -> b) -> c
c. (a -> b, c -> d) -> (a, c) -> (b, d)
d. ((a, a) -> b) -> a -> b
e. (a -> (b -> c)) -> b -> a -> c
f. (a -> b) -> (a, a) -> (b, b)
g. (a -> b, a -> c) -> a -> (b, c)
h. (a -> (b -> c)) -> (a -> b) -> a -> c
i. a -> b -> a
-}

-- Ejercicio 9) Dar expresiones equivalentes a las funciones definidas a continuación utilizando funciones como compose, flip, etc. (dadas en los ejercicios anteriores) y sin utilizar lambas.

{-
a. cuadruple x = doble (doble x)
b. timesTwoPlusThree x = suma (doble x) 3
c. fourTimes f x = f (f (f (f x)))

---------------------
a. cuadruple x = many 2 doble x
               = twice doble x
			   
b. timesTwoPlusThree x = suma (doble x) 3

c. fourTimes f x = many 4 f x
				 = many 4

-}
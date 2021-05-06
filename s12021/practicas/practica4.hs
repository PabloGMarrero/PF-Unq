--Ejercicio 1) Determinar si las siguientes funciones son parciales o totales. Justificar.
{-
a. udiv (x,y) = div x y
b.  udivE (x,0) = error "No puedo dividir por 0"
	udivE (x,y) = div x y
c. udivH = uncurry div
d. succ x = x + 1
e. succH = suma 1
f. porLaMitad = flip div 2
g. conDieresis 'u' = 'ü'
h. conDieresisB 'u' = 'ü'
   conDieresisB c = conDieresisB c
i. conTildePM 'a' = 'á'
   conTildePM 'e' = 'é'
   conTildePM 'i' = 'í'
   conTildePM 'o' = 'ó'
   conTildePM 'u' = 'ú'
j. conTildeE c = if esVocal c
   then conTildePM c
   else error "El valor recibido no es vocal"
k. conTilde c = if esVocal c && esMinuscula c
   then conTildePM c
   else c


a. es parcial cuando y es 0
b. es parcial cuando y es 0
c. es parcial cuando el y del segundo parámetro es 0
d. es total
e. es total
f. es total
g. es parcial cuando es algo distinto de 'u'
h. es parcial cuando recibe algo distinto a 'u'
i. es parcial cuando es algo distinto de una vocal
j. es parcial cuando c no es vocal
k. es total

-}

--Ejercicio 2) Para cada una de las funciones del ejercicio anterior, determinar si una o más de las otras es equivalente a ella

{- 

a, b y c son equivalentes
d y e son equivalentes
g y h son equivalentes
i y j son equivalentes
k no tiene equivalentes

-}


--Ejercicio 3) Dada la siguiente definición para la función twice, determinar cuántos y cuáles son los redexes en las siguientes expresiones.

{- twice = \f -> \x -> f (f x)

a. twice doble
b. twice doble 2
c. twice

a. tiene un redexes
	twice doble
	(\f -> \x -> f (f x)) doble
	\x -> doble (doble x)
b. tiene dos redexes
	twice doble 2
	(\f -> \x -> f (f x)) doble 2
	doble (doble 2)
	
a. tiene un redexes

-}


--Ejercicio 4) Dada la siguiente definición para la función twice, determinar cuántos y cuáles son los redexes en las siguientes expresiones.
{- 
	twice f = g
		where g x = f (f x)

a. twice doble
b. twice doble 2
c. twice

a. tiene un redexes, twice doble
b. tiene dos redexes, twice doble y doble 2
c. no tiene redexes

-}


-- Ejercicio 5) Dada la siguiente definición para la función twice, determinar cuántos y cuáles son los redexes en las siguientes expresiones.
{- 
	twice f x = f (f x)

a. twice doble
b. twice doble 2
c. twice

a. no tiene redexes
b. tiene un redexes, twice doble 2
c. no tiene redexes

-}


-- Ejercicio 6) Para cada tipo a continuación, intentar dar dos expresiones que denoten valores diferentes. Las expresiones deben ser diferentes de bottom, y en el caso de ser funciones, una debe ser total y otra debe ser parcial. De no ser posible hacer alguno de los casos, explicar por qué.
{-

a. a
b. Int -> a
c. a -> b

a. 
	bottom
b. 
	\x -> x div 0
	\x -> error "error raro..."

c. 
	\x -> const undefined x
	
-}
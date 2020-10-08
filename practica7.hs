-------------Practica 7
--Ej1
data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show
data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamon | Queso | Salsa deriving Show

--data Pizza
--regla base: Prepizza está en Pizza 
--regla ind: si p está en Pizza e i está en Ingrediente entonces Capa i p está en Pizza
--Data Ingrediente
--regla base una por cada Ingrediente.

--Ej2
--f :: Pizza -> a 
--f Prepizza = ...
--f (Capa i p) = ... i ... f p ...

--Ej3
cantidadDeCapas:: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

cantidadDeAceitunas::Pizza -> Int
cantidadDeAceitunas Prepizza = 0
cantidadDeAceitunas (Capa i p) = aceitunas i + cantidadDeAceitunas p

aceitunas::Ingrediente -> Int
aceitunas (Aceitunas n) = n
aceitunas _ = 0

duplicarAceitunas::Pizza-> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa i p) = Capa (dobleAceitunas i) (duplicarAceitunas p)

dobleAceitunas::Ingrediente -> Ingrediente
dobleAceitunas (Aceitunas n) = Aceitunas (doble n)
dobleAceitunas i = i

sinLactosa::Pizza -> Pizza
sinLactosa Prepizza = Prepizza
sinLactosa (Capa i p) = if esQueso i then sinLactosa p else Capa i (sinLactosa p)

esQueso::Ingrediente-> Bool
esQueso Queso = True
esQueso _ = False

aptaIntolerantesLactosa::Pizza -> Bool
aptaIntolerantesLactosa Prepizza = False
aptaIntolerantesLactosa (Capa i p) = not (esQueso i) && aptaIntolerantesLactosa p

conDescripcionMejorada::Pizza -> Pizza
conDescripcionMejorada Prepizza = Prepizza
conDescripcionMejorada (Capa i p) = descMejorada i (conDescripcionMejorada p)

descMejorada::Ingrediente -> Pizza -> Pizza
descMejorada (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n + m)) p
descMejorada i p = Capa i p

-------Ej 4

{--
a)
cantidadDeAceitunas Prepizza ​=​ cantidadDeAceitunas (conDescripcionMejorada Prepizza)

Lado der 
cantidadDeAceitunas (conDescripcionMejorada Prepizza)
= def conDescripcionMejorada.1
cantidadDeAceitunas Prepizza
LLEGUÉ

b)cantidadDeAceitunas (Capa Queso Prepizza)​=​ 
  cantidadDeAceitunas (conDescripcionMejorada (Capa Queso Prepizza))

Lado der
cantidadDeAceitunas (conDescripcionMejorada (Capa Queso Prepizza))
= def conDescripcionMejorada.2
cantidadDeAceitunas (descMejorada Queso (conDescripcionMejorada Prepizza))
= def conDescripcionMejorada.1
cantidadDeAceitunas (descMejorada Queso Prepizza)
= def descMejorada
cantidadDeAceitunas (Capa Queso Prepizza)
LLEGUÉ

c)
cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))​ =​ cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 8) (Capa Queso Prepizza)))

Lado der
cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 8) (Capa Queso Prepizza)))
=  def conDescripcionMejorada.2
cantidadDeAceitunas (descMejorada (Aceitunas 8) conDescripcionMejorada (Capa Queso Prepizza) )
=  def conDescripcionMejorada.2
cantidadDeAceitunas (descMejorada (Aceitunas 8) (descMejorada Queso (conDescripcionMejorada Prepizza)) )
=  def conDescripcionMejorada.1
cantidadDeAceitunas (descMejorada (Aceitunas 8) (descMejorada Queso Prepizza) )
= def descMejorada.2
cantidadDeAceitunas (descMejorada (Aceitunas 8) (Capa Queso Prepizza) )
= def descMejorada.2
cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))​ 
--}

---------SECCION II
type Nombre = String
data Planilla = Fin | Registro Nombre Planilla deriving Show
data Equipo = Becario Nombre | Investigador Nombre Equipo Equipo Equipo deriving Show
{-
Planilla
  regla base: Fin pertenece a Planilla
  regla ind: si p pertenece a Planilla entonces Registro nombre p pertenece a Planilla
Equipo
  Becario n pertenece a Equipo
  si e1, e2, e3 pertenece a Equipo entonces Investigador n e1 e2 e3 pertenece a Equipo


Ej2
f::Planilla -> a
f Fin = ..
f (Registro n p) = ... n .. f p ..

f'::Equipo-> a
f' (Becario n) = n
f' (Investigador n e1 e2 e3) = ... n ... f' e1...f' e2...f' e3...
-}

--Ej3
largoDePlanilla:: Planilla -> Int
largoDePlanilla Fin = 0
largoDePlanilla (Registro n p) = 1 + largoDePlanilla p

esta::Nombre-> Planilla -> Bool
esta n1 Fin = False
esta n1 (Registro n2 p) = n1 == n2 || esta n1 p

juntarPlanillas::Planilla-> Planilla -> Planilla
juntarPlanillas Fin p = p
juntarPlanillas (Registro n p) p2 = Registro n (juntarPlanillas p p2)

nivelesJerarquicos:: Equipo -> Int
nivelesJerarquicos (Becario nombre) = 0
nivelesJerarquicos (Investigador n e1 e2 e3) = 1 + nivelesJerarquicos e1 + nivelesJerarquicos e2 +nivelesJerarquicos e3

cantidadDeIntegrantes:: Equipo -> Int
cantidadDeIntegrantes (Becario nombre) = 1
cantidadDeIntegrantes (Investigador n e1 e2 e3) = 1 + cantidadDeIntegrantes e1 + cantidadDeIntegrantes e2 +cantidadDeIntegrantes e3

planillaDeIntegrantes::Equipo -> Planilla
planillaDeIntegrantes (Becario nombre) = Registro nombre Fin
planillaDeIntegrantes (Investigador n e1 e2 e3) = Registro n (
 juntarPlanillas (juntarPlanillas (planillaDeIntegrantes e1) (planillaDeIntegrantes e2)) (planillaDeIntegrantes e3))

 {-
Ej 4
a) ​largoDePlanilla (juntarPlanillas Fin ​p​)​=​ largoDePlanilla Fin + largoDePlanilla ​p
 
Lado izq
​largoDePlanilla (juntarPlanillas Fin ​p​)
= def juntarPlanillas.1
largoDePlanilla p

Lado der
largoDePlanilla Fin + largoDePlanilla ​p
= largoDePlanilla.1
0 + largoDePlanilla ​p
= aritmetica
largoDePlanilla ​p

b)
largoDePlanilla (juntarPlanillas (Registro "Edsger" Fin) ​p​) ​=​ largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla ​p

Lado izq
largoDePlanilla (juntarPlanillas (Registro "Edsger" Fin) ​p​)
= def juntarPlanillas.2
largoDePlanilla (Registro "Edsger" (juntarPlanillas Fin p))
= def juntarPlanillas.1
largoDePlanilla (Registro "Edsger" p)
1 + largoDePlanilla p

Lado der
largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla ​p
= def largoDePlanilla.2
1 + largoDePlanilla Fin + largoDePlanilla p
= def largoDePlanilla.1
1 + 0 + largoDePlanilla p
= aritmetica
1 + largoDePlanilla p

c)
largoDePlanilla (juntarPlanillas (Registro "Alan" (Registro "Edsger" Fin))​p​)​ =​ largoDePlanilla (Registro "Alan" (Registro "Edsger" Fin)) + largoDePlanilla ​p

Lado izq 
largoDePlanilla (juntarPlanillas (Registro "Alan" (Registro "Edsger" Fin))​ p​)​
= def juntarPlanillas.2 
largoDePlanilla (Registro "Alan" (juntarPlanillas (Registro "Edsger" Fin) p))
= def juntarPlanillas.2 
largoDePlanilla (Registro "Alan" (Registro "Edsger (juntarPlanillas Fin p)))
= def juntarPlanillas.1
largoDePlanilla (Registro "Alan" (Registro "Edsger p))
= def largoDePlanilla.2
1 + largoDePlanilla (Registro "Edsger p)
= def largoDePlanilla.2
1 + 1 + largoDePlanilla p

Lado der
largoDePlanilla (Registro "Alan" (Registro "Edsger" Fin)) + largoDePlanilla ​p
= def largoDePlanilla.2
1 + largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla ​p
= def largoDePlanilla.2
1 + 1 + largoDePlanilla Fin + largoDePlanilla ​p
= def largoDePlanilla.1
1 + 1 + 0 + largoDePlanilla ​p
= aritmetica
1 + 1 + largoDePlanilla ​p

d)
largoDePlanilla (juntarPlanillas (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin)))​p​)​ =​ largoDePlanilla (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) + largoDePlanilla ​p

Lado izq
largoDePlanilla (juntarPlanillas (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) ​p​)​
= juntarPlanillas.2
largoDePlanilla (Registro "Alonzo" (juntarPlanillas (Registro "Alan" (Registro "Edsger" Fin)) ​p​)) 
= juntarPlanillas.2
largoDePlanilla (Registro "Alonzo" (Registro "Alan" ( juntarPlanillas(Registro "Edsger" Fin)​) p))
= juntarPlanillas.2
largoDePlanilla (Registro "Alonzo" (Registro "Alan" ( juntarPlanillas(Registro "Edsger" Fin) ​p​)))
= juntarPlanillas.2
largoDePlanilla (Registro "Alonzo" (Registro "Alan" ( Registro "Edsger" (juntarPlanillas Fin p))))
=juntarPlanillas.1
largoDePlanilla (Registro "Alonzo" (Registro "Alan" ( Registro "Edsger" p)))
= largoDePlanilla.2
1 + largoDePlanilla (Registro "Alan" ( Registro "Edsger" p))
= largoDePlanilla.2
1 + 1 + largoDePlanilla ( Registro "Edsger" p)
= largoDePlanilla.2
1 + 1 + 1 +  largoDePlanilla p

Lado der
largoDePlanilla (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) + largoDePlanilla ​p
= largoDePlanilla.2
1 + largoDePlanilla (Registro "Alan" (Registro "Edsger" Fin)) + largoDePlanilla ​p
= largoDePlanilla.2
1 + 1 + largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla ​p
= largoDePlanilla.2
1 + 1 + largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla ​p
= largoDePlanilla.2
1 + 1 + 1 + largoDePlanilla Fin + largoDePlanilla ​p
= largoDePlanilla.1
1 + 1 + 1 + 0 + largoDePlanilla ​p
= aritmetica
1 + 1 + 1 +  largoDePlanilla p

-}
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
sinLactosa (Capa i p) = if esQueso i then sinLactosa p else Capa i (sinLactosa p) --agregarSinLacteo ing (sinLactosa p)
--agregarSinLacteo Queso p = p
--agregarSinLacteo ing p = Capa ing p

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

-- Ejercicio 5
{ -



-} 


---------SECCION III
data Dungeon a = Habitacion a | Pasaje (Maybe a) (Dungeon a) | Bifurcacion (Maybe a) (Dungeon a) (Dungeon a)

cantidadDeBifurcaciones::Dungeon a -> Int
cantidadDeBifurcaciones (Habitacion e) = 0
cantidadDeBifurcaciones (Pasaje m d) =  cantidadDeBifurcaciones d
cantidadDeBifurcaciones (Bifurcacion m d1 d2) = 1 + cantidadDeBifurcaciones d1 + cantidadDeBifurcaciones d2

cantidadDePuntosInteresantes::Dungeon a -> Int
cantidadDePuntosInteresantes (Habitacion e) = 1
cantidadDePuntosInteresantes (Pasaje m d) =  unoSiEsInteresante m + cantidadDePuntosInteresantes d
cantidadDePuntosInteresantes (Bifurcacion m d1 d2) = unoSiEsInteresante m + cantidadDePuntosInteresantes d1 + cantidadDePuntosInteresantes d2

unoSiEsInteresante::Maybe a -> Int
unoSiEsInteresante (Just _) = 1
unoSiEsInteresante _ = 0

cantidadDePuntosVacios::Dungeon a -> Int
cantidadDePuntosVacios (Habitacion e) = 0
cantidadDePuntosVacios (Pasaje m d) =  unoSiEsVacio m + cantidadDePuntosVacios d
cantidadDePuntosVacios (Bifurcacion m d1 d2) = unoSiEsVacio m + cantidadDePuntosVacios d1 + cantidadDePuntosVacios d2

unoSiEsVacio::Maybe a -> Int
unoSiEsVacio Nothing = 1
unoSiEsVacio _ = 0

cantidadDePuntosCon::a -> Dungeon a ->Int
cantidadDePuntosCon e (Habitacion x) =  unoSiEs e (Just x)
cantidadDePuntosCon e (Pasaje m d) =  unoSiEs e m + cantidadDePuntosCon e d
cantidadDePuntosCon e (Bifurcacion m d1 d2) = unoSiEs e m + cantidadDePuntosCon e d1 + cantidadDePuntosCon e d2

unoSiEs::a-> Maybe a -> Int
unoSiEs e (Just x) = if x==e then 1 else 0
unoSiEs e _ = 0

esLineal::Dungeon a -> Bool
esLineal (Habitacion e) = True
esLineal (Pasaje m d) = esLineal d
esLineal (Bifurcacion m d1 d2) = False && esLineal d1 && esLineal d2  -- Directamente False pero usar recursión

llenoDe:: Eq a => a -> Dungeon a -> True
llenoDe e (Habitacion e2) = e == e2
llenoDe e (Pasaje m d) = trueSiEs e m && llenoDe e d
llenoDe e (Bifurcacion m d1 d2) = trueSiEs e m && llenoDe e d1 && llenoDe e d2

trueSiEs:: a -> Maybe a -> Bool
trueSiEs e (Just e2) = e == e2
trueSiEs e _ = False

------Extra practica 7
data Camino = Izq Camino | Der Camino | Final
data Objeto = Tesoro | Chatarra
data Cofre = Cofre Objeto
data Mapa = FinMapa Cofre | Bifurcacion Cofre Mapa Mapa

--Ej1)
todosCumplen :: (Objeto -> Bool) -> Mapa -> Bool
todosCumplen p (FinMapa c) = p (objeto c)
todosCumplen p (Bifurcacion c m1 m2) = p (objeto c) &&
  todosCumplen p m1 &&
  todosCumplen p m2

objeto::Cofre-> Objeto
objeto (Cofre o) = o

--Ej2)
hayTesoroEn :: Camino -> Mapa -> Bool
hayTesoroEn Final (FinMapa c) = esTesoro (objeto c)
hayTesoroEn Final (Bifurcacion c m1 m2) = esTesoro (objeto c)
hayTesoroEn (Izq c) (FinMapa co) = False -- si es FinMapa y tengo Iqz directamente puedo dar False
hayTesoroEn (Izq c) (Bifurcacion co m1 m2) = hayTesoroEn c m1
hayTesoroEn (Der c) (FinMapa co) = False -- si es FinMapa y tengo Der directamente puedo dar False
hayTesoroEn (Der c) (Bifurcacion co m1 m2) = hayTesoroEn c m2 

esTesoro::Objeto-> Bool
esTesoro Tesoro = True
esTesoro _ = False

--Ej3)

algunoDelCaminoCumple :: (Objeto -> Bool) -> Camino -> Mapa -> Bool
algunoDelCaminoCumple p Final (FinMapa c) = p (objeto c)
algunoDelCaminoCumple p Final (Bifurcacion c m1 m2) = p (objeto c)
algunoDelCaminoCumple p (Izq c) (FinMapa c) = False -- si es FinMapa y tengo Iqz directamente puedo dar False
algunoDelCaminoCumple p (Izq c) (Bifurcacion c m1 m2) = algunoDelCaminoCumple p c m1
algunoDelCaminoCumple p (Der c) (FinMapa c) = False -- si es FinMapa y tengo Der directamente puedo dar False
algunoDelCaminoCumple p (Der c) (Bifurcacion c m1 m2) = algunoDelCaminoCumple p c m2 
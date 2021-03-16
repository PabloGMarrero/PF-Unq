data Componente = LanzaTorpedos | Motor Int | Almacen [Barril] deriving Show	
data Barril = Comida | Oxigeno | Torpedo | Combustible deriving Show
data Sector = S SectorId [Componente] [Tripulante]
type SectorId = String
type Tripulante = String

data Nave = NodeNG SectorId [Componente] [Tripulante] [Nave] deriving Show

naves = NodeNG "A" [LanzaTorpedos, Motor 1, Almacen [Torpedo, Oxigeno, Torpedo, Torpedo]] ["Pablo", "Pepe", "pepon"] 
        [
          NodeNG "B" [Motor 3, Almacen [Comida, Oxigeno, Combustible, Combustible]] ["Pablo2", "Pepe2", "pepon2"] []
        ]

foldN0 ::(SectorId -> [Componente] -> [Tripulante] -> [b]-> b) -> Nave -> b
foldN0 f (NodeNG s cs ts ns) = f s cs ts (map (foldN0 f) ns)

foldN ::(SectorId -> [Componente] -> [Tripulante] -> c -> b) -> ([b] -> c)-> Nave -> b
foldN f g (NodeNG s cs ts ns) = f s cs ts ( g (map (foldN f g) ns))

--Propósito: Devuelve todos los sectores de la nave.
sectores :: Nave -> [SectorId]
sectores = foldN (\s cs ts rns -> s: rns) concat 

--Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
--el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion :: Nave -> Int
poderDePropulsion = foldN (\s cs ts rns -> cantPropulsion cs + rns) sum

cantPropulsion:: [Componente] -> Int
cantPropulsion = foldr (\x rxs -> poderDeMotor x + rxs) 0

poderDeMotor :: Componente -> Int
poderDeMotor (Motor n) = n
poderDeMotor _ = 0

--Propósito: Devuelve todos los barriles de la nave.
barriles :: Nave -> [Barril]
barriles = foldN (\s cs ts rns ->  extraerBarriles cs ++ rns ) concat

extraerBarriles :: [Componente] -> [Barril]
extraerBarriles = foldr (\x rns -> if esAlmacen x then extraerAlmacen x ++rns else rns) []

esAlmacen::Componente -> Bool
esAlmacen (Almacen x) = True
esAlmacen _ = False

extraerAlmacen:: Componente -> [Barril]
extraerAlmacen (Almacen x) = x

--Propósito: Añade una lista de componentes a un sector de la nave.
--Nota: ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs s = recN0 (\s' cs' ts ns rns -> if s == s' then NodeNG s' (cs'++cs) ts ns else NodeNG s' cs' ts rns ) --faltaria aplanarAlmacen

recN0 ::(SectorId -> [Componente] -> [Tripulante] -> [Nave] ->[b]-> b) -> Nave -> b
recN0 f (NodeNG s cs ts ns) = f s cs ts ns (map (recN0 f) ns)

--Propósito: Incorpora un tripulante a una lista de sectores de la nave.
--Precondición: Todos los id de la lista existen en la nave.
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA t ss = recN0 (\s cs ts ns rns -> if elem s ss then NodeNG s cs (t:ts) ns else NodeNG s cs ts rns )

--Propósito: Devuelve los sectores en donde aparece un tripulante dado.
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados t = foldN (\s cs ts rns -> if elem t ts then s :rns else rns ) concat

--Propósito: Devuelve la lista de tripulantes, sin elementos repetidos.
tripulantes :: Nave -> [Tripulante]
tripulantes = foldN (\s cs ts rns -> ts ++ rns) concat 
type Presa = String -- nombre de presa

type Territorio = String -- nombre de territorio

type Nombre = String -- nombre de lobo

data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
          | Explorador Nombre [Territorio] Lobo Lobo
          | Cria Nombre

data Manada = M Lobo

foldL :: (Nombre -> [Presa] -> b -> b -> b -> b)
      -> (Nombre -> [Territorio] -> b -> b -> b)
      -> (Nombre -> b)
      -> Lobo
      -> b

foldL fcaz fe fcr (Cazador n ps l1 l2 l3) = 
	fcaz n ps
	(foldL fcaz fe fcr l1)
	(foldL fcaz fe fcr l2)
	(foldL fcaz fe fcr l3)

foldL fcaz fe fcr (Explorador n ts l1 l2) =
	fe n ts
	(foldL fcaz fe fcr l1)
	(foldL fcaz fe fcr l2)

foldL fcaz fe fcr (Cria n) = fcr n

recL :: (Nombre -> [Presa] -> Lobo -> Lobo -> Lobo -> b -> b -> b -> b)
      -> (Nombre -> [Territorio] ->  Lobo -> Lobo -> b -> b -> b)
      -> (Nombre -> b)
      -> Lobo
      -> b

recL fcaz fe fcr (Cazador n ps l1 l2 l3) = 
	fcaz n ps
	l1
	l2
	l3
	(recL fcaz fe fcr l1)
	(recL fcaz fe fcr l2)
	(recL fcaz fe fcr l3)

recL fcaz fe fcr (Explorador n ts l1 l2) =
	fe n ts
	l1
	l2
	(recL fcaz fe fcr l1)
	(recL fcaz fe fcr l2)

recL fcaz fe fcr (Cria n) = fcr n


buenaCaza :: Lobo -> Bool
buenaCaza lobo = cantAlimento lobo > cantCrias lobo

cantAlimento :: Lobo -> Int
cantAlimento = foldL (\n ps r1 r2 r3 -> length ps + r1 + r2 + r3)
                     (\n ts r1 r2 -> r1 + r2)
                     (\n -> 0)

cantCrias :: Lobo -> Int
cantCrias = foldL (\n ps r1 r2 r3 -> r1 + r2 + r3)
                  (\n ts r1 r2 -> r1 + r2)
                  (\n -> 1)

elAlfa :: Lobo -> (Nombre, Int)
elAlfa = foldL (\n ps r1 r2 r3 -> (n, length ps) `compAlfa` r1 `compAlfa` r2 `compAlfa` r3)
               (\n ts r1 r2 -> r1 `compAlfa` r2)
               (\n -> (n, 0))

compAlfa :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
compAlfa (nombre1, n1) (nombre2, n2) =
	if n1 > n2
	   then (nombre1, n1)
	   else (nombre2, n2)

losQueExploraron :: Territorio -> Lobo -> [Nombre]
losQueExploraron t = foldL (\n ps r1 r2 r3 -> r1 ++ r2 ++ r3)
                           (\n ts r1 r2 -> if elem t ts then n : r1 ++ r2 else r1 ++ r2)
                           (\n -> [])

agregarTerritorio t = foldL (\n ps r1 r2 r3 -> Cazador n ps r1 r2 r3)
                            (\n ts r1 r2 -> Explorador n (t:ts) r1 r2)
                            (\n -> Cria n)

todosLosExploradores = foldL (\n ps r1 r2 r3 -> r1 ++ r2 ++ r3)
                             (\n ts r1 r2 -> n : r1 ++ r2)
                             (\n -> [])

--------------------------------------------------------------------------------------
-- Demo, con las definiciones explicitas
losQueExploraron t (agregarTerritorio t) = todosLosExploradores (agregarTerritorio t)
--------------------------------------------------------------------------------------

exploradoresPorTerritorio :: Lobo -> [(Territorio, [Nombre])]
exploradoresPorTerritorio = foldL (\n ps r1 r2 r3 -> 
	                                              r1 
	                                              `mergeT` r2 
	                                              `mergeT` r3)

                                  (\n ts r1 r2 -> map (\t -> (t, [n])) ts 
                                  	              `mergeT` r1 
                                  	              `mergeT` r2)

                                  (\n -> [])

mergeT :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
mergeT ts1 ts2 = foldr (\(t, ns) r -> addT t ns r) ts2 ts1

addT :: Territorio -> [Nombre] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
addT t ns ts = recr (\(t', ns') xs r ->
	                     if t == t'
	                     	then (t', ns ++ ns') : xs
	                     	else (t', ns') : r)
                    [(t, ns)]
                    ts

recr = undefined


superioresDelCazador :: Nombre -> Lobo -> [Nombre]
superioresDelCazador nl = recL g
                               (\n ts l1 l2 r1 r2 -> r1 ++ r2)
                               (\n -> [])
   where g n ps l1 l2 l3 r1 r2 r3 =
   	       if elemC nl l1 || elemC nl l2 || elemC nl l3
   	       	  then n : r1 ++ r2 ++ r3
   	       	  else []

elemC n' = foldL (\n ps r1 r2 r3 -> n == n' || r1 || r2 || r3)
                 (\n ts r1 r2 -> r1 || r2)
                 (\n -> False)

---------------------------------------------------------------

noHayCazadoresPobres :: Lobo -> Bool
noHayCazadoresPobres =
	foldL (\n ps r1 r2 r3 -> length ps > 0 && r1 && r2 && r3)
          (\n ts r1 r2 -> r1 && r2)
          (\n -> True)

-- Demo

Si noHayCazadoresPobres lobo = True
entonces (cantAlimento lobo > 0)

---------------------------------------------------------------

data LoboG = CazadorG Nombre [Presa] [Lobo]
           | ExploradorG Nombre [Territorio] [Lobo]
           | CriaG Nombre

----------------------------------------------------------------

data Nave = NodeNG SectorId [Componente] [Tripulante] [Nave]

-----------------------------------------------------------------
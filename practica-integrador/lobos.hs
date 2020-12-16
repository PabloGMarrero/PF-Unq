type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo

data LoboG = Cazador Nombre [Presa] [LoboG]
           | Explorador Nombre [Territorio] [LoboG]
           | Cria Nombre

lg = Cazador "Kpo" ["", "", ""] [Cria "KpoJr", Explorador "Kpo tio" ["Rancho"] [Cazador "Kpito" [ "", ""] [] ]]

foldLG0::(Nombre -> [Presa] -> [b] -> b ) -> (Nombre -> [Territorio] -> [b] -> b) -> (Nombre -> b) -> LoboG -> b
foldLG0 fc fe fcr (Cazador no ps ls) =  fc no ps (map (foldLG0 fc fe fcr) ls)
foldLG0 fc fe fcr (Explorador no ts ls) = fe no ts (map (foldLG0 fc fe fcr) ls)
foldLG0 fc fe fcr (Cria no) = fcr no

foldLG::(Nombre -> [Presa] -> c -> b ) -> ([b] -> c) -> (Nombre -> [Territorio] -> c -> b) -> (Nombre -> b) -> LoboG -> b
foldLG fc frl fe fcr (Cazador no ps ls) =  fc no ps (frl (map (foldLG fc frl fe fcr) ls))
foldLG fc frl fe fcr (Explorador no ts ls) = fe no ts (frl (map (foldLG fc frl fe fcr) ls))
foldLG fc frl fe fcr (Cria no) = fcr no

buenaCaza :: LoboG -> Bool
buenaCaza l = cantidadLobos l < cantidadComida l

cantidadLobos::LoboG -> Int
cantidadLobos = foldLG (\no ps rls -> 1 + rls ) sum (\no ts rls -> 1 + rls ) (const 1)
--cantidadLobos = foldLG0 (\no ps rls -> 1 + sum rls) (\no ts rls -> 1 + sum rls) (const 1)

cantidadComida:: LoboG -> Int
cantidadComida = foldLG (\no ps rls -> length ps + rls) sum (\no ps rls -> rls) (const 0)


elAlfa :: LoboG -> (Nombre, Int)
elAlfa (Cazador no ps ls) = let (k, v) = maxTupla ( map elAlfa ls) in if length ps > v then (no, length ps) else (k, v)
elAlfa (Explorador no ts ls) = let (k, v) = maxTupla ( map elAlfa ls) in if v > 0 then (k, v) else (no, 0)
elAlfa (Cria no) = (no, 0)

--elAlfa' :: LoboG -> (Nombre, Int)
--elAlfa' = foldLG (\no ps rls -> rls ) maxTupla (\no ts rls-> rls) (\no-> (no, 0)) 
elAlfa' = foldLG0 (\no ps rls -> let (k, v) = maxTupla rls in if length ps > v then (no, length ps) else (k, v) ) 
                  (\no ts rls-> let (k, v) = maxTupla rls in if v > 0 then (k, v) else (no, 0)) 
                  (\no -> (no, 0))  

maxTupla :: [(Nombre, Int)] -> (Nombre, Int)
maxTupla [] = ("", 0)
maxTupla [x] = x
maxTupla xs = foldr1 (\x y -> compareTuple x y) xs

compareTuple x y =  if snd x >= snd y then x else y

losQueExploraron :: Territorio -> LoboG -> [Nombre]
losQueExploraron t = foldLG0 (\no ps rls -> concat rls) (\no ts rls -> if elem t ts then no: concat rls else concat rls) (const [])

exploradores = Explorador "Pedro" ["Canada", "Argentina", "Peru"] 
                                        [
                                        Explorador "Pedro2" ["Canada"] [],
                                        Explorador "Pedro3" ["Peru"] [],
                                        Explorador "Pedro4" ["Argentina"] []
                                        ]

territorios = [("Argentina", ["Pedro"]), ("Canada", ["Pedro"]), ("Peru", ["Pedro"]), ("Canada", ["Pedro2"]), ("Peru", ["Pedro3"]), ("Argentina", ["Pedro4"]) ]

exploradoresPorTerritorio :: LoboG -> [(Territorio, [Nombre])]
exploradoresPorTerritorio = foldLG0 (\no ps rls -> concat rls) 
                                    (\no ts rls -> aplanarTerritorios (replicateNoToTerritorios no ts ++ ( concat rls) ))
                                    (const [])

replicateNoToTerritorios:: Nombre -> [Territorio] -> [(Territorio, [Nombre])]
replicateNoToTerritorios n = foldr (\t rts -> (t, [n]) : rts) []

aplanarTerritorios::[(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
--aplanarTerritorios [x] = [x]
--aplanarTerritorios (x:xs) = aplanar x (aplanarTerritorios xs)
aplanarTerritorios = foldr (\p rps -> if isNull rps then [p] else aplanar p rps) []

isNull::[a] -> Bool
isNull [] = True
isNull _ = False

aplanar::(Territorio, [Nombre]) -> [(Territorio, [Nombre])]-> [(Territorio, [Nombre])]
aplanar (x,y) = recr [(x,y)] f
                      where f (z,w) xs r = if x == z
                                        then (x,w ++ y) : xs
                                        else (z,w) : r

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr f g []     = f
recr f g (x:xs) = g x xs (recr f g xs)
data Nota = Do | Re | Mi | Fa | Sol | La | Si deriving (Show, Eq, Ord)
type Tiempo = Int
type Duracion = Int
data Comp = Silencio Duracion | Batido Nota Duracion | Arpegio Comp Comp | Acorde Comp Comp deriving Show

type Midi = [[Nota]]
type Sampler = Tiempo -> [Nota]

cmp = Arpegio (Silencio 3) (Acorde (Batido Do 1) ( Batido Re 5))

-- Ej 1
alargar :: Int -> Comp -> Comp
alargar n (Silencio durN) = Silencio (n * durN)
alargar n (Batido no durN)= Batido no (durN * n)
alargar n (Arpegio c1 c2) = Arpegio (alargar n c1) (alargar n c2)
alargar n (Acorde c1 c2) = Acorde (alargar n c1) (alargar n c2)

duracion :: Comp -> Int
duracion (Silencio durN) = durN
duracion (Batido no durN)= durN
duracion (Arpegio c1 c2) = max (duracion c1) (duracion c2)
duracion (Acorde c1 c2) = max (duracion c1) (duracion c2)

sintetizar :: Comp -> Midi
sintetizar (Silencio durN) = [[]]
sintetizar (Batido no durN)= [[no]]
sintetizar (Arpegio c1 c2) = sintetizar c1 ++ sintetizar c2
sintetizar (Acorde c1 c2) =  sintetizar c1 ++ sintetizar c2

--Ej 2
foldCp::(Duracion -> b) -> (Nota -> Duracion -> b) -> (b -> b-> b) -> (b -> b-> b) -> Comp -> b
foldCp fs fb fa ac (Silencio durN) = fs durN
foldCp fs fb fa ac (Batido no durN)= fb no durN
foldCp fs fb fa ac (Arpegio c1 c2) = fa (foldCp fs fb fa ac  c1) (foldCp fs fb fa ac  c2)
foldCp fs fb fa ac (Acorde c1 c2) = ac (foldCp fs fb fa ac  c1) (foldCp fs fb fa ac  c2)

alargar' :: Int -> Comp -> Comp
alargar' n = foldCp (Silencio . (n*))
                    (\no durN -> Batido no (durN * n))
					Arpegio Acorde
duracion' :: Comp -> Int
duracion' = foldCp id (flip const) max max

sintetizar' :: Comp -> Midi
sintetizar' = foldCp (const [[]]) (\no durn -> [[no]] ) (++) (++)

-- Ej 3

silencios :: Comp -> [Tiempo]
silencios = foldCp (:[]) (\n dur -> []) (++) (++)

unir :: [Comp] -> [Comp] -> Comp
--unir [] [] = Silencio 0
--unir (x:xs) (y:ys) = Arpegio (Acorde x y) (unir xs ys)
unir xs ys = foldr g (Silencio 0) xs
  where g x r = map (\y -> Acorde x y) Arpegio r ys 

samplear :: Comp -> Sampler
samplear (Silencio durN) = \x -> []
samplear (Batido no durN)= \x -> if x == durN then [no] else []
samplear (Arpegio c1 c2) = \x -> (samplear c1) x ++ (samplear c2) x
samplear (Acorde c1 c2)  = \x -> (samplear c1) x ++ (samplear c2) x

superponer :: [Sampler] -> Duracion -> Midi
superponer [] d = []
superponer (f:fs) d = f d : superponer fs d



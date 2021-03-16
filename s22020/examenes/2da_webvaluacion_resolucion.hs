-- HSON, una forma de describir datos en Haskell
-- similar a JSON pero con el agregado de poder

data HSON a = HValue a
            | HList [HSON a]
            | HMap  [(String, HSON a)]
            | HOp (a -> a) (HSON a)

-----------------------------------------------------
-- 1. Recursión explícita
-----------------------------------------------------

-- Obtiene todos los nombres de campos pertenecientes
-- a los diferentes HMap
fields :: HSON a -> [String]
fields (HValue _) = []
fields (HList hs) = concat (map fields hs)
fields (HMap mhs) = map fst mhs ++ concat (map (fields . snd) mhs)
fields (HOp f h)  = fields h

-- Denota la profundidad de un HSON
depthH :: HSON a -> Int
depthH (HValue _) = 0
depthH (HList hs) = 1 + foldr max 0 (map depthH hs)
depthH (HMap mhs) = 1 + foldr max 0 (map (depthH . snd) mhs)
depthH (HOp f h)  = 1 + depthH h

-- Todos los valores donde se aplicaron las transformaciones (HOp)
-- correspondientes
values :: HSON a -> [a]
values (HValue x) = [x]
values (HList hs) = concat (map values hs)
values (HMap mhs) = concat (map (values . snd) mhs)
values (HOp f h)  = map f (values h)

mapH :: (a -> a) -> HSON a -> HSON a
mapH f (HValue x) = HValue x
mapH f (HList hs) = HList (map (mapH f) hs)
mapH f (HMap mhs) = HMap (map (\(k,v) -> (k, mapH f v)) mhs)
-- f se aplica sólo a HValue, ¿no?
-- Sino se estaría aplicando dos veces.
mapH f (HOp g h)  = HOp (f . g) (mapH f h) 

-- Elimina los HOp
normH :: HSON a -> HSON a
normH (HValue x) = HValue x
normH (HList hs) = HList (map normH hs)
normH (HMap mhs) = HMap (map (\(k,v) -> (k, normH v)) mhs)
normH (HOp g h)  = mapH g (normH h)

-----------------------------------------------------
-- 2. Definición de foldH
-----------------------------------------------------

-- El tipo viene dado
foldH :: (a -> b)
      -> ([b] -> b)
      -> ([(String, b)] -> b)
      -> ((a -> a) -> b -> b)
      -> HSON a
      -> b
foldH f g h w (HValue x)  = 
    f x

foldH f g h w (HList hs)  = 
    g (map (foldH f g h w) hs)

foldH f g h w (HMap mhs)  = 
    h (map (\(k, v) -> (k, foldH f g h w v)) mhs)

foldH f g h w (HOp f' h') = 
    w f' (foldH f g h w h')

-----------------------------------------------------
-- 3. Uso de foldH
-----------------------------------------------------

-- Defina las funciones del punto 1 usando foldH

fields' :: HSON a -> [String]
fields' = foldH (const [])
                 concat
                 (\kvs -> map fst kvs ++ concat (map snd kvs))
                 (\g r -> r)
                 -- flip const

depthH' :: HSON a -> Int 
depthH' = foldH (const 0)
                 ((+1) . foldr max 0)
                 ((+1) . foldr max 0 . map snd)
                 (const (+1))

values' :: HSON a -> [a]
values' = foldH (:[])
                 concat
                 (concat . map snd)
                 (const id)

mapH' :: (a -> a) -> HSON a -> HSON a
mapH' f = foldH HValue
                HList
                HMap
                (\g -> HOp (f . g))

-- No requiere mapH
normH' :: HSON a -> HSON a
normH' = foldH HValue
               HList
               HMap
               mapH'

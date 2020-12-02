--Un elemento de ejemplo de HSON es el siguiente

ejH = HMap [ ("name"        , HValue "Gobstones-Auth-Client")
           , ("version"     , HValue "0.1.0A")
           , ("dependencies", 
                HList [ HMap [ ("dependsOn", HValue "@Babel/Code-Frame")
                             , ("version"  , HValue "7.8.3B") 
                             ]
                      , HOp (map id) 
                            (HMap [ ("dependsOn", HValue "@babel/compat")
                                  , ("version"  , HValue "7.11.0a") 
                                 ])
                      ])
           ]
{-que representa al JSON

      { "name": "Gobstones-Auth-Client"
      , "version": "0.1.0A"
      , "dependencies": [ { "dependsOn": "@Babel/Code-Frame"
                          , "version": "7.8.3B"                }
                        , { "dependsOn": "@BABEL/COMPAT"
                          , "version": "7.11.0A"              }
                        ]
      }
Definir la función
       fields :: HSON a -> [ String ] 
que dado un HSON describe la lista con los nombres de sus campos.
Por ejemplo:   fields ejH = [ "name", "version", "dependencies"
                          , "dependsOn", "version", "dependOn", "version" ]
						  
-}

data HSON a = HValue a | HList [HSON a] | HMap  [(String, HSON a)] | HOp (a -> a) (HSON a)

fields::HSON a -> [ String ] 
fields (HValue a) = []
fields (HList hs) = concatMap fields hs 
fields (HMap ps)  = arrPairToStr ps 
fields (HOp f e)  = fields e 

arrPairToStr :: [(String, HSON a)] -> [String]
arrPairToStr [p] = fst p : fields (snd p)
arrPairToStr (p:ps) = fst p : fields (snd p) ++ arrPairToStr ps

arrToStr :: [HSON a] -> [String]
arrToStr [h] = fields h
arrToStr (h:hs) = fields h ++ arrToStr hs


-- Ej 2
{-
values ejH = [ "Gobstones-Auth-Client", "0.1.0A"
                          , "@Babel/Code-Frame", "7.8.3B"
                          , "@BABEL/COMPAT", "7.11.0A" ]
-}

values :: HSON a -> [a]
values (HValue a) = [a]
values (HList hs) = concatMap values hs 
values (HMap ps)  = obtainValues ps 
values (HOp f e)  = values e

obtainValues::[(String, HSON a)] -> [a]
obtainValues [] = []
obtainValues (p:ps) = values (snd p) ++ obtainValues ps


-- Ej 3
{-que dado un HSON describe la profundidad del mismo.
Por ejemplo:   depth ejH = 5-}

depth :: HSON a -> Int
depth (HValue a) = 0
depth (HList hs) = 1 + sum (map depth hs)
depth (HMap ps)  = 1 + obtainDepth ps
depth (HOp f e)  = 1 + depth e

obtainDepth::[(String, HSON a)] -> Int
obtainDepth [] = 0
obtainDepth (p:ps) = depth (snd p) + obtainDepth ps

{- Ej 4 

Definir la función
       nroFields :: HSON a -> Int
que dado un HSON describe la cantidad de campos que tiene el mismo.
Por ejemplo:   nroFields ejH = 7

-}

nroFields :: HSON a -> Int
nroFields (HValue a) = 0
nroFields (HList hs) = sum (map nroFields hs)
nroFields (HMap ps)  = obtainFields ps
nroFields (HOp f e)  = nroFields e

obtainFields::[(String, HSON a)] -> Int
obtainFields [] = 0
obtainFields (p:ps) = 1 + nroFields (snd p) + obtainFields ps



-- Ej 5

{- mapH :: (a->a) -> HSON a -> HSON a
que dada una función de transformación de los elementos, describe el HSON que resulta de modificar los valores del HSON dado según la función dada. El resultado debe tener la misma estructura que el original (mismos constructores), y NO debe realizar más trabajo del necesario.
Por ejemplo:    
     mapH (map toLower) ejH = 
       HMap [ ("name" , HValue "gobstones-auth-client")
            , ("version" , HValue "0.1.0a")
            , ("dependencies", 
                 HList [ HMap [ ("dependsOn", HValue "@babel/code-frame")
                              , ("version" , HValue "7.8.3b") ]
                              , HOp (map toLower . map toUpper) 
                                    (HMap [ ("dependsOn", HValue "@babel/compat") 
                                          , ("version" , HValue "7.11.0a") 
                                          ])
                              ])
            ] 
-}

mapH :: (a->a) -> HSON a -> HSON a
mapH f (HValue a) = HValue (f a)
mapH f (HList hs) = HList (map (mapH f) hs)
mapH f (HMap ps)  = applyFunPairs f ps
mapH f (HOp f' e)  = HOp f' (mapH f e)

applyFunPairs:: (a->a) -> [(String, HSON a)] -> HSON a
applyFunPairs f [p] =  mapH f (snd p)
applyFunPairs f (p:ps) =  applyFunPairs f ps


{-applyTransf :: (Name, Content -> Content) -> FileSystem -> FileSystem
applyTransf (nom, fcont) =
    foldFS (\nom' cont ->
              if nom == nom'
                 then File nom' (fcont cont)
                 else File nom' cont) 
           (\nom' rs   -> Folder nom rs)
		   
-}


--Ej 6

normH :: HSON a -> HSON a
normH (HValue a) = HValue a
normH (HList hs) = HList hs
normH (HMap ps)  = HMap ps
normH (HOp f e)  = normHOp f (normH e)

normHOp::(a->a) -> HSON a -> HSON a
normHOp f (HValue a) = HValue (f a)
normHOp f (HList hs) = HList hs
normHOp f (HMap ps) = applyFunPairs f ps
normHOp f e = mapH f e

-- Ej 7 definir fold

foldH :: (a -> b) -> 
 ([b] -> b) -> 
 ([(String, b)] -> b) -> 
 ((a -> a) -> b -> b) -> 
 HSON a -> 
 b
foldH fv fl fm fo (HValue e) = fv e
foldH fv fl fm fo (HList hs) = fl (map (foldH fv fl fm fo) hs)
foldH fv fl fm fo (HMap ps) = fm (foldr (\e es -> (fst e, foldH fv fl fm fo (snd e)) :es ) [] ps)
foldH fv fl fm fo (HOp f e ) = fo f (foldH fv fl fm fo e)


--Ej 8 definr todo usando fold

fields'::HSON a -> [ String ] 
--fields' = foldH (const []) (\xs -> xs ) (\ps -> arrPairToStr ps ) (\f re -> re)
fields' = foldH (const []) id (\ps -> arrPairToStr ps ) (flip const)

values'::HSON a -> [ String ] 
values' = foldH (\a -> [a]) (\rhs -> rhs) (\ps -> obtainValues ps) id

depth' :: HSON a -> Int
depth' =  foldH (const 0) (\rhs -> 1 + sum rhs) (\ps -> 1 ) (\re -> 1 + re)

nroFields' :: HSON a -> Int
nroFields' = foldH (const 0) (\hs -> sum hs) (\ps -> obtainFields ps) id

mapH':: (a->a) -> HSON a -> HSON a
mapH' f = (\e -> HValue (f e)) (\hs -> HList (map (mapH f) hs)) (\ps -> applyFunPairs f ps) (\f' e -> HOp f' (mapH f e))

normH' :: HSON a -> HSON a
normH' = foldH HValue HList HMap (\f re -> normHOp f re)


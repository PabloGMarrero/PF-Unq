import Data.Char(isLower, toUpper)
data QueryHDB a = Project [String] (QueryHDB a)  -- La lista de strings son los valores de las claves que querés quedarte "proyectame los nombres"
                | AnySatisfies (a -> Bool) (QueryHDB a)
                | AllSatisfies (a -> Bool) (QueryHDB a)
                | This
type HongoDB a = [HSON a]
data HSON a = HValue a
            | HList [HSON a]
            | HMap [(String, HSON a)]
            | HOp (a -> a) (HSON a)
--Un elemento de ejemplo de HSON es el siguiente

ejH = HMap [ ("name" , HValue "Gobstones-Auth-Client")
           , ("version" , HValue "0.1.0A")
           , ("dependencies",
               HList [ HMap [ ("dependsOn", HValue "@Babel/Code-Frame")
                            , ("version" , HValue "7.8.3B")
                            ]
                     , HOp (map toUpper)
                           (HMap [ ("dependsOn", HValue "@babel/compat")
                                 , ("version" , HValue "7.11.0a")
                                 ])
                     ])
           ]
{-
que representa al JSON

      { "name": "Gobstones-Auth-Client"
      , "version": "0.1.0A"
      , "dependencies": [ { "dependsOn": "@Babel/Code-Frame"
                          , "version": "7.8.3B" }
                        , { "dependsOn": "@BABEL/COMPAT"
                          , "version": "7.11.0A" }
                        ]
      }
	  -}
-- Considerar la siguiente función
values :: HSON a -> [a]
values (HValue x) = [x]
values (HList hs) = concatMap values hs
values (HMap khs) = concatMap (values . snd) khs
values (HOp f h)  = map f (values h)

-- dada una query y una base de datos devuelve el resultado de la consulta sobre dicha base de datos.
--Puede resultar útil definir una función auxiliar proj :: [String] -> HSON a -> HSON a para manejar los casos de las proyecciones.
evalQ :: QueryHDB a -> HongoDB a -> HongoDB a
evalQ (Project vl q) hdb = map (proj vl) (evalQ q hdb)        
evalQ (AnySatisfies f q) hdb = filter (\x -> any f (values x) ) (evalQ q hdb)
evalQ (AllSatisfies f q) hdb = filter (\x -> all f (values x) ) (evalQ q hdb)
evalQ (This) hdb = hdb
--evalQ (Project ss q)     h = map (proj ss) (evalQ q h)
--evalQ (AnySatisfies p q) h = (if anyQ p (evalQ q h) then (h++) else id) (evalQ q h) --if anyQ p (evalQ q h) then h ++ (evalQ q h) else (evalQ q h)
--evalQ (AllSatisfies p q) h = (if allQ p (evalQ q h) then (h++) else id) (evalQ q h) --if allQ p (evalQ q h) then h ++ (evalQ q h) else (evalQ q h)
--evalQ This               h = h

anyQ::(a -> Bool) -> HongoDB a -> Bool
anyQ p h = any p (concatMap values h)

allQ::(a -> Bool) -> HongoDB a -> Bool
allQ p h = all p (concatMap values h)

proj :: [String] -> HSON a -> HSON a
proj [] h = HList []
proj (x:xs) h = proj xs h    --no es asi pero no logro comprenderlo


{-Hola
Decime
Paablitohoy a las 19:06
Buenas, no estoy entiendo bien el constructor Project, que intenta representar
Fidel MLhoy a las 19:06
Una proyección de valores
"Los valores que cumplen estar entre los dados"
Perdón, las claves que cumplen estar entre las dadas
Paablitohoy a las 19:07
Es decir la lista de strings son valores que están en esa query ?
Bueno, claves entonces
Fidel MLhoy a las 19:07
La lista de strings son los valores de las claves que querés quedarte
"proyectame los nombres"
¿Se entiende?
Paablitohoy a las 19:08
oook ahi un poco más claro quedó, lo pienso de esa manera
Si, mejor que antes
chas gracias-}

query1 = Project ["version", "dependencies"] This
query2 = AnySatisfies (any isLower) This



--Ej 2
{- Escribir una función normQ :: QueryHDB a -> QueryHDB a, que normaliza una consulta a la base de datos. Una consulta está normalizada si no tiene proyecciones consecutivas, ni predicados de satisfacción universal anidados, ni predicados de satisfacción existencial anidados.-}
normQ :: QueryHDB a -> QueryHDB a
normQ (Project ss q) = normProject (Project ss (normQ q))
normQ (AnySatisfies p q) = normAny (AnySatisfies p (normQ q))
normQ (AllSatisfies p q) = normAll (AllSatisfies p (normQ q))
normQ (This) = This

normProject:: QueryHDB a -> QueryHDB a
normProject (Project ss (Project ss' q)) = Project (ss++ss') q 
normProject q = q

normAny:: QueryHDB a -> QueryHDB a
normAny (AnySatisfies p (AnySatisfies p' q)) = AnySatisfies (projectFun p p' (||)) q
normAny q = q

projectFun :: (a -> Bool) -> (a -> Bool) -> (Bool -> Bool -> Bool) -> (a -> Bool)
projectFun pred1 pred2 f = (\x -> f (pred1 x)  (pred2 x))

normAll:: QueryHDB a -> QueryHDB a
normAll (AllSatisfies p (AllSatisfies p' q)) = AllSatisfies (projectFun p p' (&&)) q
normAll q = q

-- Ej 3

fields :: HSON a -> [String]
fields (HValue x) = []
fields (HList hs) = concatMap fields hs
fields (HMap khs) = map fst khs ++ concatMap (fields . snd) khs
fields (HOp f h)   = fields h

nroFields :: HSON a -> Int
nroFields (HValue x) = 0
nroFields (HList hs) = sum (map nroFields hs)
nroFields (HMap khs) = length khs + sum (map (nroFields . snd) khs)
nroFields (HOp f h)  = nroFields h

{-
Demostración
¿length . fields = nroFields?

Por ppio de extensionalidad sea h un Hson a cualquiera

¿(length . fields) h = nroFields h?

Por def de (.)

¿length (fields h) = nroFields h?

Por ppio de inducción en la estructura de h

- Caso base  h =  HValue x
	¿length (fields (HValue x)) = nroFields (HValue x)?
	
- Caso ind.1  h =  HList h'
	TI: ¿length (fields (HList h')) = nroFields (HList h')?
	HI: ¡length (fields h') = nroFields h'!
	
- Caso base.2  h =  HMap ps
	¿length (fields (HMap ps)) = nroFields (HMap ps)?
	
- Caso ind.3  h =  HOp f h'
	TI: ¿length (fields (HOp f h')) = nroFields (HOp f h')?
	HI: ¡length (fields h') = nroFields h'!

Demostración

- Caso base.1
	LADO IZQ
	length (fields (HValue x))
	= fields.1
	length []
	= length.1
	0
	= nroFields.1
	nroFields (HValue x)
	
	LLEGUÉ
	
- Caso ind 1
	LADO IZQ
	length (fields (HList h'))
	= fields.2
	length (concatMap fields h')
	= def concatMap
	length (concat (map fields h'))
	= PROPIEDAD length-concat
	sum (map length (map fields h') )
	= por def (.)
	sum ((map length . map fields ) h')
	= PROPIEDAD fusión de map
	sum (map (length . fields) h')
	= def (.)
	sum (map (length (fields h'))
	= HI
	sum ( map nroFields h')
	
	LLEGUÉ
	
	LADO DER
	nroFields (HList h')
	= nroFields .2
	sum (map nroFields h')
	
	LLEGUÉ
	
- Caso base.2
	LADO IZQ
	length (fields (HMap ps)) 
	= fields.3
	length (map fst ps ++ concatMap (fields . snd ) ps )
	= PROPIEDAD distributividad de length sobre (++)
	length (map fst ps) + length (concatMap (fields . snd ) ps )
	= PROPIEDAD map conserva length
	length ps + length (concatMap (fields . snd ) ps )
	= def concatMap
	length ps + length (concat (map (fields . snd ) ps ) ) 
	= PROPIEDAD length-concat 
	length ps + sum (map length (map (fields . snd ) ps ))
	= PROPIEDAD map conserva length
	length ps + sum (map length ps) 
	
	LADO DER
	nroFields (HMap ps)
	= nroFields.3
	length ps + sum ( map (nroFields . snd) ps ) 

	
	
- Caso ind.3 
	LADO DER
	length (fields (HOp f h')) 
	= fields.4
	length (fields h')
	= HI
	nroFields h'
	= nroFields.4
	nroFields (HOp f h')
	
	LLEGUÉ
	
-}

{-
LEMA 1: para todo f. para todo g. para todo xs.
          si se cumple que
              para todo x en xs. f x = g x
          entonces map f xs = map g xs

LEMA 2: para todo f. (snd . (\(x,y) -> (x, f y))) = (f . snd)

PROPIEDAD (fusión de map):
        para todo f. para todo g. (map f . map g) = map (f . g)

PROPIEDAD (length-concat):
        para todo xss. length (concat xss) = sum (map length xss)

PROPIEDAD (conmutación concat-map):
        para todo f. para todo xss. concat (map (map f) xss) = map f (concat xss)

PROPIEDAD (distributividad de length sobre (++)):
        para todo xs. para todo ys. length (xs++ys) = length xs + length ys

PROPIEDAD (map conserva length):
        para todo f. para todo xs. length (map f xs) = length xs
-}



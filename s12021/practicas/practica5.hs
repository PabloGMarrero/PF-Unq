-- Ejercicio 1) Dadas las siguientes definiciones determinar el tipo de las expresiones:

data Gusto = Chocolate | DulceDeLeche | Frutilla | Sambayon
data Helado = Vasito Gusto | Cucurucho Gusto Gusto | Pote Gusto Gusto Gusto
chocoHelate consH = consH Chocolate

{- 
a. Vasito
b. Chocolate
c. Cucurucho
d. Sambayón
e. Pote
f. chocoHelate
g. chocoHelate Vasito
h. chocoHelate Cucurucho
i. chocoHelate (Cucurucho Sambayon)
j. chocoHelate (chocoHelate Cucurucho)
k. chocoHelate (Vasito DulceDeLeche)
l. chocoHelate Pote
m. chocoHelate (chocoHelate (Pote Frutilla))

RTA

a. Gusto -> Helado
b. Gusto
c. Gusto -> (Gusto -> Helado)
d. Gusto
e. Gusto -> (Gusto -> (Gusto -> Helado))
f. (Gusto -> Helado ) -> Helado
g. Helado
h. Gusto -> Helado
i. Helado
j. Helado
k. bottom
l. Gusto -> (Gusto -> Helado)
m. Helado

-}


-- Ejercicio 2) Dado el siguiente tipo que pretende representar dígitos binarios, definir las funciones:

data DigBin = O | I

{- 
a. dbAsInt :: DigBin -> Int, que dado un símbolo que representa un dígito binario lo transforma en su significado como número.
b. dbAsBool :: DigBin -> Bool, que dado un símbolo que representa un dígito binario lo transforma en su significado como booleano.
c. dbOfBool :: Bool -> DigBin, que dado un booleano lo transforma en el símbolo que representa a ese booleano.
d. negDB :: DigBin -> DigBin, que dado un dígito binario lo transforma en el otro.
-}

dbAsInt :: DigBin -> Int
dbAsInt O = 0
dbAsInt I = 1

dbAsBool :: DigBin -> Bool
dbAsBool O = False
dbAsBool I = True

dbOfBool :: Bool -> DigBin
dbAsBool False = O
dbAsBool True  = I

negDB :: DigBin -> DigBin
dbAsInt O = I
dbAsInt I = O 


-- Ejercicio 3) Dado el siguiente tipo que pretende representar dígitos decimales, definir las siguientes funciones:
data DigDec = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9

{-
a. ddAsInt :: DigDec -> Int, que dado un símbolo que representa un dígito decimal lo transforma en su significado como número.
b. ddOfInt :: Int -> DigDec, que dado un número entre 0 y 9 lo transforma en el símbolo que representa a ese dígito.
c. nextDD :: DigDec -> DigDec, que dado un dígito decimal lo transforma en el siguiente según el orden circular dado en la definición.
d. prevDD :: DigDec -> DigDec, que dado un dígito decimal lo transforma en el anterior según el orden circular dado en la definición.
-}

ddAsInt :: DigDec -> Int
ddAsInt D0 = 0
ddAsInt D1 = 1
ddAsInt D2 = 2
ddAsInt D3 = 3
ddAsInt D4 = 4
ddAsInt D5 = 5
ddAsInt D6 = 6
ddAsInt D7 = 7
ddAsInt D8 = 8
ddAsInt D9 = 9

ddOfInt :: Int -> DigDec
ddOfInt 0 = D0
ddOfInt 1 = D1
ddOfInt 2 = D2
ddOfInt 3 = D3
ddOfInt 4 = D4
ddOfInt 5 = D5
ddOfInt 6 = D6
ddOfInt 7 = D7
ddOfInt 8 = D8
ddOfInt 9 = D9

nextDD::DigDec->DigDec
nextDD D0 = D1 
nextDD D1 = D2
nextDD D2 = D3
nextDD D3 = D4
nextDD D4 = D5
nextDD D5 = D6
nextDD D6 = D7
nextDD D7 = D8
nextDD D8 = D9
nextDD D9 = D0

prevDD::DigDec->DigDec​
prevDD D0 = D9 
prevDD D1 = D0
prevDD D2 = D1
prevDD D3 = D2
prevDD D4 = D3
prevDD D5 = D4
prevDD D6 = D5
prevDD D7 = D6
prevDD D8 = D7
prevDD D9 = D8


--Ejercicio 4) Dado el tipo definir las funciones

data Medida = Mm Float | Cm Float | Inch Float | Foot Float

{-
a. asMm :: Medida -> Medida, que dada una medida cualquiera la transforma en una medida en milímetros que aproxima la dada según la conversión establecida.
b. asCm :: Medida -> Medida, que dada una medida cualquiera la transforma en una medida en centímetros que aproxima la dada según la conversión establecida.
c. asInch :: Medida -> Medida, que dada una medida cualquiera la transforma en una medida en pulgadas que aproxima la dada según la conversión establecida.
d. asFoot :: Medida -> Medida, que dada una medida cualquiera la transforma en una medida en pies que aproxima la dada según la conversión establecida.
-}

asMm::Medida->Medida​
asMm (Cm n) = Mm (n*0.1)
asMm (Inch n) = Mm (n*0.039)
asMm (Foot n) = Mm (n*0.003)
asMm mm = mm

asCm::Medida->Medida
asCm (Mm n) = Mm (n*10)
asCm (Inch n) = Mm (n*0.394)
asCm (Foot n) = Mm (n*0.033)
asCm cm = cm

asInch::Medida->Medida
asInch (Mm n) = Inch (n*25.4)
asInch (Cm n) = Inch (n*2.54)
asInch (Foot n) = Inch (n*0.083)
asInch inch = inch

asFoot::Medida->Medida​
asFoot (Mm n) = Foot (n*304.8)
asFoot (Cm n) = Foot (n*30.48)
asFoot (Inch n) = Foot (n*12)
asFoot f = f 


-- Ejercicio 5) Determinar el tipo de las siguientes expresiones en base a la siguiente definiciones
data Shape = Circle Float | Rect Float Float
construyeShNormal :: (Float -> Shape) -> Shape
construyeShNormal c = c 1.0

{-
a. uncurry Rect
b. construyeShNormal (flip Rect 5.0)
c. compose (uncurry Rect) swap
d. uncurry Cucurucho
e. uncurry Rect swap
f. compose uncurry Pote
g. compose Just
h. compose uncurry (Pote Chocolate)


RTA
a. (Float, Float) -> Shape
b. Shape
c. (Float, Float) -> Shape
d. (Gusto, Gusto) -> Helado
e. bottom
f. Gusto -> (Gusto, Gusto) -> Helado
g. (c->a) -> c -> Maybe a

    compose::(a->b) -> (c->a) -> c -> b
    Just:: a-> Maybe a
    a = a 
    b = Maybe a
h. bottom

    compose::(a->b)   -> (c->a) -> c -> b
    uncurry::(a->b->c)-> (a, b) -> c
    (Pote Chocolate)::Gusto->Gusto->Helado
    uncurry (Pote Chocolate)::(Gusto,Gusto)->Helado
	
-}


-- Ejercicio 6) Para cada una de las expresiones del ejercicio anterior que denoten funciones, construir una expresión aplicándola.
{-
a. uncurry Rect (3.0, 1.0)
b. ya es expresión
c. compose (uncurry Rect) swap  (7.5, 3.5)
d. uncurry Cucurucho (Frutilla, Chocolate)
e. es bottom
f. compose uncurry Pote Frutilla (Frutilla, Chocolate)
g. compose Just (\x->True) 3
h. es bottom
-}


--Ejercicio 7) Definir la función tryCatch según los siguientes tipos

data MayFail a = Raise Exception | Ok a
data Exception = DivByZero | NotFound | NullPointer | Other String
type ExHandler a = Exception -> a

tryCatch :: MayFail a -> (a -> b) -> ExHandler b -> b
tryCatch (Raise Exception) f g = g a
tryCatch (Ok a) f g = f a


--PRactica 5
data Gusto = Chocolate | DulceDeLeche | Frutilla | Sambayon deriving Show
data Helado = Vasito Gusto | Cucurucho Gusto Gusto | Pote Gusto Gusto Gusto deriving Show

chocoHelate consH = consH Chocolate

--Ej1 dar el tipo de las sig expresiones

Vasito::Gusto-> Helado
Chocolate::Gusto
Cucurucho::Gusto-> Gusto-> Helado
Sambayón::Gusto
Pote::Gusto->Gusto-> Gusto-> Helado
chocoHelate::(Gusto -> Helado)-> Helado
chocoHelate Vasito::Helado
chocoHelate Cucurucho::Gusto -> Helado
chocoHelate (Cucurucho Sambayon)::Helado
chocoHelate (chocoHelate Cucurucho)::Helado
chocoHelate (Vasito DulceDeLeche)::bottom
chocoHelate Pote::Gusto->Gusto->Helado
chocoHelate (chocoHelate (Pote Frutilla))::Helado


--Ej2) Dado el tipo definir las funciones
data DigBin = O | I

dbAsInt::DigBin->Int
dbAsInt O = 0
dbAsInt I = 1

dbAsBool::DigBin->Bool
dbAsBool O = False
dbAsBool I = True

dbOfBool::Bool->DigBin​
dbOfBool False = O
dbOfBool True = I

negDB::DigBin->DigBin
negDB O = I
negDB I = O

--Ej3) Dado el tipo definir las funciones
data DigDec = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9

ddAsInt::DigDec->Int​
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

ddOfInt::Int->DigDec
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

--Ej4) Dado el tipo definir las funciones
data Medida = Mm Float | Cm Float | Inch Float | Foot Float

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

--Ej5) DEterminar el tipo de las funciones según estas definiciones

data Shape = Circle Float | Rect Float Float deriving Show
construyeShNormal :: (Float -> Shape) -> Shape
construyeShNormal c = c 1.0

{-
a.uncurry Rect::(Float,Float)->Shape
b.construyeShNormal (flip Rect 5.0)::Shape
c.compose (uncurry Rect) swap::(Float,Float)->Shape
d.uncurry Cucurucho::(Gusto, Gusto)->Helado
e.uncurry Rect swap::bottom
f.compose uncurry Pote::Gusto -> (Gusto,Gusto) -> Helado
g.compose Just::(c->a) -> c -> Maybe a
    compose::(a->b) -> (c->a) -> c -> b
    Just::a-> Maybe a
    a = a 
    b = Maybe a
h.compose uncurry (Pote Chocolate)::bottom
    compose::(a->b)   -> (c->a) -> c -> b
    uncurry::(a->b->c)-> (a, b) -> c
    (Pote Chocolate)::Gusto->Gusto->Helado
    uncurry (Pote Chocolate)::(Gusto,Gusto)->Helado
-}

--Ej7) Definir la función tryCatch según los siguientes tipos

data MayFail a = Raise Exception | Ok a
data Exception = DivByZero | NotFound | NullPointer | Other String
type ExHandler a = Exception -> a

tryCatch :: MayFail a -> (a -> b) -> ExHandler b -> b
tryCatch (Raise Exception) f g = g a
tryCatch (Ok a) f g = f a

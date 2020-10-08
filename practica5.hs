--PRactica 5
data Gusto = Chocolate | DulceDeLeche | Frutilla | Sambayon deriving Show
data Helado = Vasito Gusto | Cucurucho Gusto Gusto | Pote Gusto Gusto Gusto deriving Show

chocoHelate consH = consH Chocolate

data Shape = Circle Float | Rect Float Float deriving Show
construyeShNormal :: (Float -> Shape) -> Shape
construyeShNormal c = c 1.0
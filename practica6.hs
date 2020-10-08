--Practica 6
--Ej1) demostrar las sig prop
{-
a)doble ​=​ \x -> 2 * x
Ppio de ext:  para todo x. doble x ​=​ (\x -> 2 * x) x

Lado izq
doble x
= def doble x<-x
x + x

Lado der 
(\x -> 2 * x) x
= beta
2 * x
= aritmetica
x + x

b)compose doble doble ​=​ cuadruple
Ppio de ext: para todo x. compose doble doble x ​=​ cuadruple x

Lado izq 
compose doble doble x 
= def compose f<-doble, g<- doble, x<-x
doble (doble x)
= def doble
doble (x + x)
= def doble
(x + x) + (x + x)
= aritmetica
x + x + x + x

Lado der
cuadruple x
= def cuadruple
4 * x
= aritmetica
x + x + x + x

Ej2) demostrar 
a) para todo ​x​. para todo ​y​. ​x​ && ​y​​=​ not ((not ​x​) || (not ​y​))
Demostración por casos
    i) x,y = True
    True && True =​ not ((not ​True​) || (not True​))

    Lado izq
    True && True
    = def &&
    True

    Lado der
    not ((not ​True​) || (not True​))
    = def not x2
    not (False || False)
    = def ||
    not False
    = def not
    True

    ii)x =True, y=False

    Lado izq
    True && False
    = def &&
    False

    Lado der
    not ((not ​True​) || (not False))
    = def not x2
    not (False || ​True​)
    = def ||
    not ​True​
    = def not
    False

    --Hacer lo mismo para cuando x= False, y = True ; x, y = False

Ej3) demostrar
a) curry suma' ​=​ suma
Ppio de ext: para todo x,y  curry suma' x y ​=​ suma x y

Lado izq
curry suma' x y
= def curry f<- suma', x<-x, y<-y
suma' (x, y)
= def suma'
x + y
= def suma
suma x y

d) uncurry suma ​=​ suma'
Ppio de ext: para todo z uncurry suma z ​=​ suma' z
Sea z un par (x, y) cualquiera:  uncurry suma (x, y) ​=​ suma' (x, y)

Lado izq
uncurry suma (x, y)
= def uncurry f<- suma , (x, y)<-(x, y)
suma x y
= def suma
x + y
= def suma'
suma' (x, y)

Ej4) y Ej5) muy similares a los anteriores

Ej6) Demostrar según las funciones assoc y appAssoc

assoc :: (a,(b,c)) -> ((a,b),c)
assoc (x,(y,z)) = ((x,y),z)

appAssoc :: (((a,b),c) -> d) -> (a,(b,c)) -> d
appAssoc f p = f (assoc p)

para todo ​f​.​appAssoc (uncurry (uncurry ​f​)) ​=​ uncurry (compose uncurry ​f​)
Por ppio de ext para todo (x, (y,z))  
        ​appAssoc (uncurry (uncurry ​f​))  (x, (y,z)) ​=​ uncurry (compose uncurry ​f​) (x, (y,z))

Lado izq
​appAssoc (uncurry (uncurry ​f​)) (x, (y,z))
= def appAssoc f<-(uncurry (uncurry ​f​)), p<-(x, (y,z)) 
(uncurry (uncurry ​f​)) (assoc (x, (y,z)) )
= def assoc
(uncurry (uncurry ​f​)) ((x,y),z)
= def uncurry f<-f,   (x, y)<-((x,y),z)
uncurry f (x, y) z
= def uncurry f<-f,  (x, y)<-(x, y)
f x y z

Lado der
uncurry (compose uncurry ​f​) (x, (y,z))
= def compose f<-uncurry , g<-f ,  x<-(x, (y,z))
uncurry (uncurry f (x, (y,z)) )
= def uncurry f<- f  , x<-(x, (y,z))
uncurry f x (y, z)
= def uncurry 
f x y z

-}

--Ej 7) Dada la siguiente definición  (f . g) x = f (g x) definir el equivalente de las  sig funciones

i)cuadruple

cuadruple'::Int -> Int
cuadruple' = doble . doble 

ii)doble
doble'::Int -> Int
doble' = (*2) . (*1)

iii) twice
twice''::(a->a)->(a->a)
twice'' f =  f . f

iv)many :: Int -> (a -> a) -> a -> a      
many 0 f x = x      
many n f x = f (many (n-1) f x)

many'::Int -> (a->a) -> (a -> a)
many' 0 f = id
many' n f = f . many' (n-1) f

{-
b) Demostrar
    i) para todo ​f​. para todo ​g​.  ​f​ . ​g​​=​ compose ​f​​ g
    Ppio de ext: para todo x (f​ . ​g​​) x=​ compose ​f​​ g x

    Lado izq 
    (f​ . ​g) x
    = def (.)
    f (g x)
    = def compose
    compose ​f​​ g x

    ii)swap . swap ​=​ id
    Ppio de ext: para todo (x,y) (swap . swap) (x,y) ​=​ id (x,y)

    Lado izq
    (swap . swap) (x,y) 
    = def (.)
    swap (swap (x,y) ) 
    = def swap
    swap (y,x)
    = def swap
    (x,y)
    = def id
    id (x,y)

    iii) para todo ​f​. para todo ​g​. para todo ​h​  ​f​ . (​g​ . ​h​) ​=​ (​f​ . ​g​) . ​h
    Ppio de ext: para todo x  ​(f​ . (​g​ . ​h​) ​) x =​ ((​f​ . ​g​) . ​h) x

    Lado izq 
    (f​ . (​g​ . ​h​) ​) x
    = def (.) f<-f, g <-(g.h)
    f ( (g.h) x)
    = def (.) f<-g , g <- g, x<- x2
    f ( g (h x))

    Lado der
    ((​f​ . ​g​) . ​h) x
    = def (.) f<- (f .g) , g<- h , x<-x
    (f . g) (h x)
    = def (.) f<-f , g<- g, x<-(h x)
    f ( g (h x)) 

    v)para todo ​f​. ​appAssoc ​f​​ =​​ f​ . assoc 
    Ppio de ext: (x, (y, z))    appAssoc ​f (x, (y, z))​​ =​​ (f​ . assoc) (x, (y, z)) 

    Lado izq
    appAssoc ​f (x, (y, z))​​ 
    = def appAssoc
    f (assoc (x, (y, z)) )
    = def (.)
    (f . assoc) (x, (y, z))

    c) Demostrar solo usando lemas en esta práctica
        i) doble . doble = cuadruple

        Lado izq
        cuadruple
        = Lema ej1 b
        compose doble doble
        = Lema ej7bi
        doble . doble

        ii)para todo ​f'​. ​curry (uncurry (curry ​f'​)) ​=​ curry ​f'

        Lado izq 
        ​curry (uncurry (curry ​f'​))
        = lema 5b
        curry f'




-}
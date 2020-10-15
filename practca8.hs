-----Seccion I

--Ej1)

length::[a]->Int
length [] =  0
length (x:xs) = 1 + length xs

sum::[Int]-> Int
sum [] = 0
sum (x:xs) = x + sum xs

product::[Int]->Int
product [] = 1
product (x:xs) = x * product xs

concat'::[[a]]->[a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

elem'::Eq a=> a-> [a]->Bool
elem' e [] = False
elem' e  (x:xs) = x == e ||  elem' e xs

any'::(a->Bool) -> [a]->Bool
any' p [] = False
any' p  (x:xs) = p x || any' p xs

all'::(a->Bool)-> [a]->Bool
all' p [] = True
all' p  (x:xs) = p x &&  all' p xs

count::(a->Bool)->[a]->Int
count p [] = 0
count p (x:xs) = unoSiCumple p x +  count p xs

unoSiCumple::(a->Bool)->a-> Int
unoSiCumple p e = if p e then 1 else 0

subset::Eq a=> [a]->[a]-> Bool
subset [] ys = True
subset (x:xs) ys = elem' x ys && subset xs ys

(++)'::[a]->[a]->[a]
(++)' [] ys = ys
(++)' (x:xs) ys = x : (++)' xs ys

reverse'::[a]->[a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip'::[a]->[b]->[(a,b)]
zip' [x] [y] = [(x, y)] 
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

unzip'::[(a,b)]->([a],[b])
unzip' [] = ([],[])
unzip' (p:ps) = let (xs, ys) = unzip' ps in (fst p:xs, snd p: ys)   

{-Ej2
a) para todo ​xs​. para todo ​ys​.​length (​xs​ ++ ​ys​) ​=​ length ​xs​ + length ​ys

sea xs una lista cualquiera
Caso base xs = []         length ([] ++ ​ys​) ​=​ length [] + length ​ys
Caso ind xs = (x:xs')    length (​(x:xs') ++ ​ys​) ​=​ length (x:xs') + length ​ys
        HI: length (​xs' ++ ​ys​) ​=​ length xs' + length ​ys

Demostración
-Caso base
    length ([] ++ ​ys​)
    = (++).1
    length ys


    length [] + length ​ys
    = length.1
    0 + length ys
    = aritmetica
    length ys

-Caso ind
    length (​(x:xs') ++ ​ys​) ​=​ length (x:xs') + length ​ys

    length (​(x:xs') ++ ​ys​)
    = (++).2
    length (x : (++) xs ys)
    =
    1 + length ((++) xs ys)
    = HI
    1 + length xs' + length ​ys

    length (x:xs') + length ​ys
    = length.2
    1 + length xs' + length ys



b.count (const True) ​=​ length
Por ppio de extensionalidad sea xs una lista
count (const True) xs ​=​ length xs

Sea xs una lista cualquiera

-Caso base []   count (const True) [] ​=​ length []
-Caso ind (x:xs')  count (const True) (x:xs') ​=​ length (x:xs')
    TI: count (const True) xs' ​=​ length xs'

Demostración

-Caso base
    count (const True) []
    = count.1
    0
    = length.1
    length []

-Caso ind
    count (const True) (x:xs')
    = count.2
    unoSiCumple (const True) x + count (const True) xs'
    =def noSiCumple
    if const True x then 1 else 0 + count (const True) xs'
    = 
    1 + count (const True) xs'
    = HI
    1 + length xs'
    
    length (x:xs')
    = length.2
    1 + length xs'

c) elem ​=  ​any . (==)
Por ppio de extensionalidad sea e un elemento cualquiera y xs una lista cualquiera
elem e ​xs =  ​(any . (==)) e  xs

Por def (.)
elem e ​xs =  ​any (==e) xs

-Caso base []   elem e [] =  ​any (==e)  []
-Caso ind (x:xs')  elem e ​(x:xs') =  ​any (==e) (x:xs')
    TI: elem ​xs' e =  ​any (==e)  xs'

Demostración
-Caso base
    elem e [] 
    = elem.1
    False

    any (==e) []
    =any.1
    False

-Caso ind
    elem e ​(x:xs')
    =elem.2
    e == x || elem e xs'
    
    any (==e) (x:xs')
    =any.2
    (==e) x || any (==e) xs'
    = 
    e == x || any (==e) xs'
    =HI
    e == x || elem e xs'

d)para todo ​x​. ​any (elem ​x​) ​=​ elem ​x​ . concat
Por ppio de extensionalidad sea xs una lista cualquiera
​any (elem ​x​) xs ​=​ (elem ​x​ . concat) xs

Por def (.)
​any (elem ​x​) xs ​=​ elem ​x​ (concat xs)

Por ppio de inducción en la estructura de la listas

-Caso base xs = []  ​​any (elem ​x​) [] ​=​ elem ​x​ (concat [])
-Caso ind  xs = (xs':xss)   any (elem ​x​) (xs':xss)  ​=​ elem ​x​ (concat (xs':xss) )
        TI any (elem ​x​) xss  ​=​ elem ​x​ (concat xss)

Demostración

-Caso base
    any (elem ​x​) []
    =any.1
    False

    elem ​x​ (concat [])
    =concat.1
    elem x []
    =elem.1
    False

-Caso ind
    any (elem ​x​) (xs':xss) 
    =any.2
    elem x xs' || any (elem ​x​) xss
    = HI
    elem x xs' || elem ​x​ (concat xss)
    
    elem ​x​ (concat (xs':xss) )
    = concat.2
    elem x (xs ++ concat xss)
    = LEMA 1
    elem x xs' || any (elem ​x​) xss
    
    Lema 1
    elem x xs' || elem ​x​ xss' = elem x (xs ++ xss')

    Por ppi de inducción en la estructura de xss'
    -Caso base xss' = []   elem x xs || elem ​x​ [] = elem x (xs ++ [])
    -Caso ind xss' = (xs':xss'')  elem x xs' || elem ​x​ (xs':xss'') = elem x (xs ++ (xs':xss''))
                    TI elem x xs' || elem ​x​ xss'' = elem x (xs ++ xss'')

    Demostración
    -Caso base
        elem x xs || elem ​x​ []
        = elem.1
        elem x xs || False
        = boolean
        elem x xs

        elem x (xs ++ [])
        = (++).1
        elem x xs
    -Caso ind
        elem x xs' || elem ​x​ (xs:xss'')
        =elem.2
        elem x xs' || (x == xs || elem x xss'')
        = bool
        elem x xs' || x == xs || elem x xss''
        = conmutativa
        elem x xs' || elem x xss'' || x == xs
        = HI
        elem x (xs ++ xss'') || x == xs
        

        elem x (xs ++ (xs':xss''))
        = (++).2
        elem x (xs' : (++) xs xss'')
        = elem.2
        x == xs' || elem x ((++) xs xss'')
        = conmutativa e infijo
        elem x(xs ++ xss'') || x == xs'
    
    TI elem x xs' || elem ​x​ xss'' = elem x (xs ++ xss'')
-}
TI any (elem ​x​) xss  ​=​ elem ​x​ (concat xss)

elem'::Eq a=> a-> [a]->Bool
elem' e [] = False
elem' e  (x:xs) = x == e ||  elem' e xs

(++)'::[a]->[a]->[a]
(++)' [] ys = ys
(++)' (x:xs) ys = x : (++)' xs ys

concat'::[[a]]->[a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

any'::(a->Bool) -> [a]->Bool
any' p [] = False
any' p  (x:xs) = p x || any' p xs

all'::(a->Bool)-> [a]->Bool
all' p [] = True
all' p  (x:xs) = p x &&  all' p xs
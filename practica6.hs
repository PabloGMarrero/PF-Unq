--Practica 6
--Ej 7
cuadruple::Int -> Int
cuadruple x = x * 4

cuadruple'::Int -> Int
cuadruple' = doble . doble 

doble'::Int -> Int
doble' = (*2) . (*1)

twice''::(a->a)->(a->a)
twice'' f =  f . f


many'::Int -> (a->a) -> (a -> a)
many' 0 f = id
many' n f = f . many' (n-1) f

many :: Int -> (a -> a) -> a -> a      
many 0 f x = x      
many n f x = f (many (n-1) f x)

suma' (x, y) = x + y
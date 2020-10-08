first (x,y) = x
swap (x, y) = (y, x)
uflip f = g where g p = f (swap p)
twice f = g where g x = f (f x)
doble  x = x + x
appDup f = g  where g x = f (x, x)
appFork (f, g) = h  where h x = (f x, g x)

appPar (f, g) = h  where h (x, y) = (f x, g y)
appDist f = g  where g (x, y) = (f x, f y)
subst f = h  where h g = k    where k x = (f x) (g x)

par = (True, 4) 

curry' =  \f -> (\x-> (\y-> f(x, y) ))
uncurry' = \f-> (\(x,y) -> f x y )
flip' f x y = (f y) x

compose f g x = f (g x)
apply f = g  where g x = f x
twice' f = g  where g x = f (f x)
id' = \x -> x

udiv (x, y) = div x y
porLaMitad = flip div 2
succH = suma 1

suma = \x-> \y-> x + y
succ' x = x + 1
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Monadas where
-- Una mónada es una estructura paramétrica M, 
-- con dos operaciones destacadas, denominadas "return" y "bind"
-- cuyo propósito es expresar algún *efecto* y su manipulación
--
--  + el tipo M agrega la información necesaria para expresar algún *efecto*
--  + la operación (return x) agrega a x la información para expresar el 
--    efecto nulo
--  + la operación (bind m k) trabaja en dos niveles
--    - en el nivel de datos, vincula el dato producido por la primera mónada m
--      a la entrada de la continuación k, permitiendo dependencia de datos
--    - en el nivel de efectos, combina los efectos de ambas mónadas mediante 
--      una operación asociativa
--
-- Las operaciones monádicas cumplen ciertas reglas que expresan la intuición
-- explicada antes.
--
-- En Haskell, adicionalmente, se agrega una operación de fail a las mónadas.
{- -- Esto va comentado porque es Haskell estándar
class Monad m where
   return :: a -> m a
   (>>=)  :: m a -> (a -> m b) -> m b
   fail   :: String -> m a
-}
-- La clase Monad permite que en Haskell todas las mónadas compartan el mismo
--  nombre para las operaciones elementales de return, bind y fail.
-- (Recordemos que en Haskell el sistema de clases es una extensión del sistema 
--  de tipos que permite que muchos tipos distintos implementen diferentes
--  versiones para una función con el mismo nombre.)
-- ATENCIÓN: esto es Haskell98 y Haskell 2010, 
--  pero NO GHC desde la versión 7.10 (se requieren un par de restricciones más)


-- Una mónada con error es una mónada que permite fallar de forma controlada
-- (en Haskell podemos usar el fail de Monad, pero si queremos expresarlo 
--  como clase debemos pensar en otra operación, por ejemplo throw...)    
class Monad m => ErrorMonad m where
  throw :: String -> m a
  -- Las clase de las mónadas con error expresa que las mónadas que son de esa
  -- clase, saben implementar la operación mencionada.

-- Una mónada que sabe imprimir es una mónada que permite acumular mensajes 
-- de alguna forma. Para eso vamos a usar una operación que llamaremos printf
class Monad m => PrintMonad m where   
  printf :: String -> m ()
  -- Las clase de las mónadas con printf expresa que las mónadas que son de esa 
  -- clase, saben implementar la operación mencionada.

--
-- A PARTIR DE ACÁ ES BONUS
--

-- Una mónada que expresa la dependencia de un contexto (sin modificarlo) 
--  es una mónada que puede ejecutarse usando el valor del contexto, y que
--  puede consultarlo
class Monad m => ReaderMonad r m | m -> r where
  ask   :: m r
  runRM :: m a -> r -> a
   -- Para expresar esto con una clase, hay que usar dos herramientas nuevas: 
   --   1. clases de múltiples parámetros
   --   2. dependencias funcionales
   -- 1. Una clase de múltiples parámetros es una clase que en lugar de 
   --    agrupar tipos, agrupa tuplas de tipos.
   --    Por ejemplo, la mónada Reader agrupa pares (r,m), donde m es el tipo
   --    de la mónada, y r el tipo del contexto
   -- 2. Una dependencia funcional establece que un tipo dado determina de
   --    forma única a otro.
   --    En este caso, para poder instanciar a un par específico (R,M) como
   --    instancia de Reader, tiene que existir un único R por cada M.
   --    Por ejemplo, (r, (r->)): se observa que si sabemos que M es (r->), 
   --    existe una única elección posible para R (o sea, r)

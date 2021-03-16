module MiFalla where

-- Una alternativa a Maybe, para mostrar que puedo (no utilizada en Main)

import Monadas
  
data MiFalla a = Falla | Anda a
     deriving Show

instance Monad MiFalla where 
  return x = Anda x
  m >>= k = case m of
               Falla -> Falla 
               Anda v -> k v
  fail msg = Falla

instance Monad MiFalla where
  throw msg = Falla  

-- No tiene sentido hacer que MiFalla sea instancia de PrintMonad

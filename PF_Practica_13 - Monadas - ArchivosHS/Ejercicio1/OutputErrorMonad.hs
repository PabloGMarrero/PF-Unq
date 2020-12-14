module OutputErrorMonad where

-- COMPLETAR con una definición de tipo 
-- de forma tal que sea al mismo tiempo 
-- instancia de ErrorMonad y de PrintMonad

import Monadas

type Screen = String    
data OutputErrMonad a = OErMn (Screen, a) | Throw String deriving Show

instance Monad OutputErrMonad where
  return x = OErMn ("", x)
  m >>= k  = case m of
              Throw s -> Throw s
              OErMn(scr1, a1) -> case k a1 of 
                                Throw s' -> Throw s'
                                OErMn (scr2, a2) -> OErMn (scr1 ++ scr2, a2)
  fail msg = Throw msg

instance PrintMonad OutputErrMonad where
 printf msg = OErMn (msg ++ "\n", ())  

instance ErrorMonad OutputErrMonad where
 throw msg = Throw msg
module MaybeMonad where

import Monadas   

{- -- Haskell Standard
data Maybe a = Nothing | Just a

instance Monad Maybe where
   return x = Just x
   m >>= k  = case m of
                Nothing -> Nothing
                Just v  -> k v
   fail _   = Nothing

-- No tiene sentido hacer que Maybe sea instancia de PrintMonad
-}    

instance ErrorMonad Maybe where
  throw msg = Nothing
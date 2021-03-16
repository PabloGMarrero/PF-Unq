module ReaderMonad where

-- Una implementación de una mónada Reader
import Monadas
import Control.Applicative 
import Control.Monad (liftM, ap) 

data Reader r a = R (r -> a)

instance Monad (Reader r) where
   return x = R (\r -> x)
   m >>= k  = R (\r -> 
                   let R fm = m
                    in let R fk = k (fm r)
                        in fk r)

instance ReaderMonad r (Reader r) where
  ask = R (\r -> r)
  runRM m r = let R fm = m
               in fm r
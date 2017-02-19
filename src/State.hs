module State
  ( State(..)
  , get
  , put
  ) where

import Control.Monad (ap, liftM)

newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  m >>= f = State $ \s ->
    let (a', s') = (runState m) s
    in runState (f a') s'

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Functor (State s) where
  fmap = liftM

put :: s -> State s ()
put s' = State $ \_ -> ((), s')

get :: State s s
get = State $ \s -> (s, s)

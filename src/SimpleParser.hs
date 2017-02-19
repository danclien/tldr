module SimpleParser
  ( SimpleParser
  , alphaNum
  , char
  , parse
  , string
  ) where

import Data.Char (isAlphaNum)
import Control.Monad (ap, join, liftM)
import Control.Applicative (Alternative(..))

newtype SimpleParser a =
  SimpleParser { unSimpleParser :: String -> [(a, String)] }

parse :: SimpleParser a -> String -> [(a, String)]
parse parser input = unSimpleParser parser $ input

instance Monad SimpleParser where
  return a = SimpleParser $ \input -> [(a, input)]
  m >>= f  = SimpleParser $ \input ->
               let g (result, output) = parse (f result) output
               in join $ fmap g $ parse m input

instance Alternative SimpleParser where
  empty   = SimpleParser $ \_     -> []
  a <|> b = SimpleParser $ \input ->
              case parse a input of
                [] -> parse b input
                a' -> a'

instance Applicative SimpleParser where
  pure = return
  (<*>) = ap

instance Functor SimpleParser where
  fmap = liftM

char :: Char -> SimpleParser Char
char c = SimpleParser $ \input ->
  case input of
    []      -> []
    (x:xs)  -> if c == x
               then [(x, xs)]
               else []

alphaNum :: SimpleParser Char
alphaNum = SimpleParser $ \input ->
  case input of
    [] -> []
    (x:xs) -> if isAlphaNum x
              then [(x, xs)]
              else []

string :: String -> SimpleParser String
string "" = return ""
string (x:xs) = do
  _ <- char x
  _ <- string xs
  return (x:xs)

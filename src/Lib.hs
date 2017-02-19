module Lib
  ( normalize
  ) where

import Control.Applicative ((<|>), some, many)
import Control.Monad (forM_)
import Data.List (intersperse)
import SimpleParser (SimpleParser, alphaNum, char, parse, string)
import State (State(..), get, put)

data PathItem = PathSegment String
              | Seperator
              | Up
              | Current
              deriving (Eq, Show)

normalize :: String -> String
normalize = pathItemsToString . normalizePathItems . parseString

parseString :: String -> [PathItem]
parseString = fst . head . parse pathItems

-- Use 'State' to build a stack of 'PathSegement's and 'Up's
normalizePathItems :: [PathItem] -> [PathItem]
normalizePathItems items =
  let filtered = filter (\item -> (item /= Seperator) && (item /= Current)) items
      f a = do
        s <- get
        case a of
          ps@(PathSegment _) -> put (ps:s)
          Up -> case s of
            []     -> return ()
            (_:xs) -> do
              put xs
              return ()
          _ -> return ()
      m = forM_ filtered f
      normalized = snd $ runState m []
      intersperseSeperators = intersperse Seperator normalized
      withEndingSeperator = if hasEndingSeperator items
                            then Seperator:intersperseSeperators
                            else intersperseSeperators
  in Seperator:(reverse withEndingSeperator)


hasEndingSeperator :: [PathItem] -> Bool
hasEndingSeperator [] = False
hasEndingSeperator xs = last xs == Seperator

-- Convert to 'String'
pathItemsToString :: [PathItem] -> String
pathItemsToString = concat . fmap pathItemToString

pathItemToString :: PathItem -> String
pathItemToString (PathSegment s) = s
pathItemToString Seperator       = "/"
pathItemToString Up              = ".."
pathItemToString Current         = "."

-- Parsers
pathSegment :: SimpleParser PathItem
pathSegment = PathSegment <$> (some alphaNum)

seperator :: SimpleParser PathItem
seperator = fmap (const Seperator) (char '/')

up :: SimpleParser PathItem
up = (const Up) <$> (string "..")

current :: SimpleParser PathItem
current = (const Current) <$> (char '.')

pathItems :: SimpleParser [PathItem]
pathItems = many (pathSegment <|> seperator <|> up <|> current)

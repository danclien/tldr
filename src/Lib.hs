module Lib
  ( normalize
  ) where

import SimpleParser
import Control.Applicative

data PathItem = PathSegment String
              | Seperator
              | Up
              | Current
              deriving (Eq, Show)

normalize :: String -> String
normalize = pathItemsToString . prependSeperatorIfNeeded . normalizeUntilDone . fst . head . parse pathItems

-- Repeat until '..'s are gone
normalizeUntilDone :: [PathItem] -> [PathItem]
normalizeUntilDone input =
  let result = normalizePathItems input
  in if any (\item -> item == Up) result
     then normalizeUntilDone result
     else result

normalizePathItems :: [PathItem] -> [PathItem]
normalizePathItems = normalizePathItems' . prependSeperatorIfNeeded . gotoFirstPathSegment

-- Traverse the list and remove one level of continuous '..'
normalizePathItems' :: [PathItem] -> [PathItem]
normalizePathItems' []                                    = []
normalizePathItems' (Seperator:(Current:xs))              = normalizePathItems' xs
normalizePathItems' (Seperator:(_:(Seperator:(Up:xs))))   = normalizePathItems' xs
normalizePathItems' (x:xs)                                = x:(normalizePathItems' xs)

-- Removes need for special cases in normalizePathItems where the path starts with '.'s or '..'s
gotoFirstPathSegment :: [PathItem] -> [PathItem]
gotoFirstPathSegment input = dropWhile (isNotPathSegment) input

isNotPathSegment :: PathItem -> Bool
isNotPathSegment (PathSegment _) = False
isNotPathSegment _               = True

-- Removes need for special cases in normalizePathItems where the path doesn't need a leading slash
prependSeperatorIfNeeded :: [PathItem] -> [PathItem]
prependSeperatorIfNeeded []              = []
prependSeperatorIfNeeded a@(Seperator:_) = a
prependSeperatorIfNeeded xs              = (Seperator:xs)

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

{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE InstanceSigs #-}
-- The above pragma temporarily disables warnings about Parser constructor and runParser not being used

module Parser
  ( -- * Important note
    -- 
    -- | The implementation of 'Parser' is intentionally
    -- hidden to other modules to encourage use of high level
    -- combinators like 'satisfy' and the ones from 'ParserCombinators'
    Parser
  , parse
  , parseMaybe
  , satisfy
  , Error(..)
  , Position(..)
  , Parsed(..)
  , Input
  ) where

import Control.Applicative
import Data.List (nub)

-- | Value annotated with position of parsed input starting from 0
data Position a = Position Int a
 deriving (Show, Eq)

-- | Parser input encapsulating remaining string to be parsed with current position
type Input = Position String

-- | Parsing error
data Error =
    Unexpected Char -- ^ Unexpected character
  | EndOfInput      -- ^ Unexpected end of input
 deriving (Show, Eq)

-- | Parsing result of value of type @a@
data Parsed a =
    Parsed a Input           -- ^ Successfully parsed value of type @a@ with remaining input to be parsed
  | Failed [Position Error]  -- ^ Failed to parse value of type @a@ with accumulated list of errors
 deriving Show

-- | Parser of value of type @a@
newtype Parser a = Parser { runParser :: Input -> Parsed a }

-- | Runs given 'Parser' on given input string
parse :: Parser a -> String -> Parsed a
parse (Parser ipa) s = ipa (Position 0 s) 

-- | Runs given 'Parser' on given input string with erasure of @Parsed a@ to @Maybe a@
parseMaybe :: Parser a -> String -> Maybe a
parseMaybe p s = case parse p s of
  Parsed a _ -> Just a
  Failed   _ -> Nothing 

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser pa) = Parser $ \i ->
    case pa i of
      Parsed a irest -> Parsed (f a) irest
      Failed e       -> Failed e

instance Applicative Parser where
  pure :: a -> Parser a
  pure a  = Parser $ \i -> Parsed a i
  
    -- \i -> pab i :: Input -> Parsed (a -> b)
  -- Parsed ab irest -> apply to a in Parsed a
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser pab) <*> (Parser pa) = Parser $ \i ->
    case pab i of
      Failed e1        -> Failed e1
      Parsed ab irest  -> case pa irest of
        Failed e2         -> Failed e2
        Parsed a iirest   -> Parsed (ab a) iirest
      

instance Alternative Parser where
  empty = Parser $ \_ -> Failed mempty
  -- Note: when both parsers fail, their errors are accumulated and *deduplicated* to simplify debugging
  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser pa1) <|> (Parser pa2) = Parser $ \i -> 
    case pa1 i of
        p1@(Parsed _ _)  -> p1
        Failed e1        -> case pa2 i of
          p2@(Parsed _ _)  -> p2
          Failed e2        -> Failed $ nub $ e1 <> e2

-- | Parses single character satisfying given predicate
--
-- Usage example:
--
-- >>> parse (satisfy (>= 'b')) "foo"
-- Parsed 'f' (Position 1 "oo")
-- >>> parse (satisfy (>= 'b')) "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (satisfy (>= 'b')) "abc"
-- Failed [Position 0 (Unexpected 'a')]
-- >>> parse (satisfy (>= 'b')) ""
-- Failed [Position 0 EndOfInput]
--
satisfy :: (Char -> Bool) -> Parser Char
satisfy check = Parser $ \(Position pos i) ->
  case i of
    [] -> Failed [Position pos EndOfInput]
    (ch : chs) -> if check ch 
      then Parsed ch (Position (succ pos) chs)
      else Failed [Position pos $ Unexpected ch]

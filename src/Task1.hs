{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

import Parser
import Data.Char (isDigit)
import ParserCombinators (char)
import Control.Applicative


-- | Parses natural number (including zero)
--
-- Usage example:
--
-- >>> parse nat "0"
-- Parsed 0 (Input 1 "")
-- >>> parse nat "123"
-- Parsed 123 (Input 3 "")
-- >>> parse nat "-123"
-- Failed [PosError 0 (Unexpected '-')]
-- >>> parse nat "abc"
-- Failed [PosError 0 (Unexpected 'a')]
-- >>> parse nat "123abc"
-- Parsed 123 (Input 3 "abc")
--

digit :: Parser Char
digit = satisfy isDigit

digits :: Parser String
digits = some digit

nat :: Parser Integer
nat =  read <$> digits
-- | Parses integer number
--
-- Usage example:
--
-- >>> parse int "0"
-- Parsed 0 (Input 1 "")
-- >>> parse int "123"
-- Parsed 123 (Input 3 "")
-- >>> parse int "-123"
-- Parsed (-123) (Input 4 "")
-- >>> parse int "abc"
-- Failed [PosError 0 (Unexpected 'a')]
-- >>> parse int "123abc"
-- Parsed 123 (Input 3 "abc")
--

makeNegative :: Parser Integer
makeNegative = char '-' *> fmap negate nat


int :: Parser Integer
int = makeNegative <|> nat

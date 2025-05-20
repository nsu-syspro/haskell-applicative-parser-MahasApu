{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

import Parser (Parser)
import ParserCombinators ( char, choice, spaces, string, oneOf )
import Task1 ( digit, nat )
import Data.List (elemIndex)
import Control.Applicative ((<|>), empty)


-- | Date representation
--
-- Date parts are expected to be in following ranges
--
-- 'Day' in @[1..31]@
-- 'Month' in @[1..12]@
-- 'Year' is any non-negative integer
--
data Date = Date Day Month Year
  deriving (Show, Eq)

newtype Day   = Day   Int deriving (Show, Eq)
newtype Month = Month Int deriving (Show, Eq)
newtype Year  = Year  Int deriving (Show, Eq)

-- | Parses date in one of three formats given as BNF
--
-- @
-- date ::= dotFormat | hyphenFormat | usFormat
--
-- dotFormat ::= day "." month "." year
-- hyphenFormat ::= day "-" month "-" year
-- usFormat ::= monthName " " usDay " " year
--
-- usDay ::= nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- day ::= "0" nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- month ::= "0" nonZeroDigit | "10" | "11" | "12"
-- year ::= number
--
-- number ::= digit | number digit
-- digit ::= "0" | nonZeroDigit
-- nonZeroDigit ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
--
-- monthName ::= "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"
-- @
--
-- Usage example:
--
-- >>> parse date "01.01.2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "12.12.2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "12-12-2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "Dec 12 2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 11 "")
-- >>> parse date "Jan 1 2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "Feb 31 2012"
-- Parsed (Date (Day 31) (Month 2) (Year 2012)) (Input 11 "")
-- >>> parse date "12/12/2012"
-- Failed [PosError 2 (Unexpected '/'),PosError 0 (Unexpected '1')]
--

nonZeroDigit :: Parser Char
nonZeroDigit = oneOf ['1'..'9']

day :: Parser Day
day =   readDay $ decade '0' nonZeroDigit

usDay :: Parser Day
usDay = readDay $ (: []) <$> nonZeroDigit

readDay :: Parser String -> Parser Day
readDay p = Day . read <$> (dayChoice <|> p)


dayChoice :: Parser String
dayChoice = choice
  [ decade '1' digit,
    decade '2' digit,
    string "30",
    string "31"
  ]

decade :: Char -> Parser Char -> Parser String
decade ch p = do
  d1 <- char ch
  d2 <- p
  pure [d1, d2]

month :: Parser Month
month = Month . read <$> choice 
  [ decade '0' nonZeroDigit,
    string "10", 
    string "11",
    string "12"
  ]


monthNames :: [String]
monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

choiceStrings :: [String] -> Parser String
choiceStrings = choice . map string

lookupMonth :: [String] -> String -> Parser Month
lookupMonth names = maybe empty (pure . Month . succ) . (`elemIndex` names)

monthName :: Parser Month
monthName = choiceStrings monthNames >>= lookupMonth monthNames

year :: Parser Year
year = Year . fromIntegral <$> nat

format :: Char -> Parser Date
format ch = do
  d <- day
  _ <- char ch
  m <- month
  _ <- char ch
  Date d m <$> year

usFormat :: Parser Date
usFormat = do
  m <- monthName
  _ <- spaces
  d <- usDay
  _ <- char ' '
  Date d m <$> year

dotFormat :: Parser Date
dotFormat = format '.'

hyphenFormat :: Parser Date
hyphenFormat = format '-'

date :: Parser Date
date = choice [dotFormat, hyphenFormat, usFormat]

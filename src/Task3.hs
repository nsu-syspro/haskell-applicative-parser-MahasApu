{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where
import Task1 (digit)
import Task2 (nonZeroDigit)
import Parser (parse, satisfy, Parsed(..), Parser)
import ParserCombinators (char, string, oneOf, choice)

import Data.Char (toLower, isSpace)
import Control.Applicative ((<|>), Alternative (..))
import Data.Functor (($>))

import Data.List (intercalate)

-- | JSON representation
--
-- See <https://www.json.org>
--
data JValue =
    JObject [(String, JValue)]
  | JArray [JValue]
  | JString String
  | JNumber Double
  | JBool Bool
  | JNull
 deriving (Show, Eq)


-- Some utility functions

makeString :: Parser Char -> Parser String
makeString = fmap (:[])

space :: Parser String
space = many $ satisfy isSpace

isValidStringChar :: Char -> Bool
isValidStringChar = (`notElem` ['"', '\\'])

parseBody :: Parser l -> Parser r -> Parser a -> Parser a
parseBody l r p = l *> p <* r

lexeme :: Parser a -> Parser a
lexeme = parseBody space space

sepByComma :: Parser a -> Parser [a]
sepByComma p = do
  first <- p
  rest <- many (space *> char ',' *> space *> lexeme p)
  pure (first : rest) 
  <|> pure []


-- Parser combinators

parseValue :: Parser JValue
parseValue = choice
  [ parseJObject,
    parseArray,
    parseJstring,
    parseNumber,
    parseJBool,
    parseJnull
  ]

parseJnull :: Parser JValue
parseJnull = JNull <$ string "null"

parseJBool :: Parser JValue
parseJBool = choice
  [ string "true"  $> JBool True,
    string "false" $> JBool False
  ]

parseElement :: Parser JValue
parseElement = lexeme parseValue

elements :: Parser [JValue]
elements = sepByComma parseElement

parseMember :: Parser (String, JValue)
parseMember = do
  s <- lexeme jstring
  _ <- lexeme $ char ':'
  e <- lexeme parseElement
  pure (s, e)

members :: Parser [(String, JValue)]
members = sepByComma parseMember

parseJObject :: Parser JValue
parseJObject = JObject <$> parseBody (lexeme $ char '{') (lexeme $ char '}') members

parseArray :: Parser JValue
parseArray   = JArray  <$> parseBody (lexeme $ char '[') (lexeme $ char ']') elements


-- JString Parser and some utils for it

jstring :: Parser String
jstring =  parseBody (char '"') (char '"') parseChars

parseJstring :: Parser JValue
parseJstring = JString <$> jstring

parseChars :: Parser String
parseChars = concat <$> many parseCharString

parseCharString :: Parser String
parseCharString = normalCharString <|> escapeSequenceString

normalCharString :: Parser String
normalCharString = makeString $ satisfy isValidStringChar

escapeSequenceString :: Parser String
escapeSequenceString = (:) '\\' <$> (char '\\' *> parseEscapeBody)

parseEscapeBody :: Parser String
parseEscapeBody = simpleEscape <|> unicodeEscape

simpleEscape :: Parser String
simpleEscape = makeString $ oneOf ['"', '\\', '/', 'b', 'f', 'n', 'r', 't']

unicodeEscape :: Parser String
unicodeEscape = do
  _  <- char 'u'
  d1 <- hexDigit
  d2 <- hexDigit
  d3 <- hexDigit
  d4 <- hexDigit
  pure $ 'u' : [d1, d2, d3, d4]

hexDigit :: Parser Char
hexDigit = satisfy isHexDigit

isHexDigit :: Char -> Bool
isHexDigit = (`elem` (['0'..'9'] <> ['a'..'f'] <> ['A'..'F']))


-- Some utils for JNumber


sign :: Parser (Double -> Double)
sign = (char '-' $> negate) <|> pure id

nonZeroInt :: Parser String
nonZeroInt = (:) <$> nonZeroDigit<*> many digit

intPart :: Parser String
intPart = (char '0' $> "0") <|> nonZeroInt

fracPart :: Parser String
fracPart = do
  dot <- char '.'
  ds  <- some digit
  pure (dot : ds)

expPart :: Parser String
expPart = do
  e   <-  oneOf ['e', 'E']
  s   <- (char '+' <|> char '-') <|> pure '+'
  ds  <-  some digit
  pure (e : s : ds)

optionalPart :: Parser String -> Parser String
optionalPart = (<|> pure "")

fracOrEmpty :: Parser String
fracOrEmpty = optionalPart fracPart

expOrEmpty :: Parser String
expOrEmpty  = optionalPart expPart


-- JNumber Parser

parseNumber :: Parser JValue
parseNumber = JNumber <$> do
  sgn <- sign
  i   <- intPart
  f   <- fracOrEmpty
  e   <- expOrEmpty
  let str = i <> f <> e
  pure $ sgn (read str)


-- | Parses JSON value-
--
-- See full grammar at <https://www.json.org>
--
-- Usage example:
--
-- >>> parse json "{}"
-- Parsed (JObject []) (Input 2 "")
-- >>> parse json "null"
-- Parsed JNull (Input 4 "")
-- >>> parse json "true"
-- Parsed (JBool True) (Input 4 "")
-- >>> parse json "3.14"
-- Parsed (JNumber 3.14) (Input 4 "")
-- >>> parse json "{{}}"
-- Failed [PosError 0 (Unexpected '{'),PosError 1 (Unexpected '{')]
--
json :: Parser JValue
json = parseElement

-- * Rendering helpers

-- | Renders given JSON value as oneline string
render :: JValue -> String
render = concatMap readable . renderTokens
  where
    -- Adds some nice spacing for readability
    readable ":" = ": "
    readable "," = ", "
    readable s   = s

-- | Renders given JSON value as list of separate tokens ready for pretty printing
renderTokens :: JValue -> [String]
renderTokens JNull        = ["null"]
renderTokens (JBool b)    = [map toLower $ show b]
renderTokens (JNumber d)  = [show d]
renderTokens (JString s)  = ["\"" ++ s ++ "\""]
renderTokens (JArray xs)  = ["["] ++ intercalate [","] (map renderTokens xs) ++ ["]"]
renderTokens (JObject xs) = ["{"] ++ intercalate [","] (map renderPair xs) ++ ["}"]
 where
  renderPair :: (String, JValue) -> [String]
  renderPair (k, v) = ["\"" ++ k ++ "\""] ++ [":"] ++ renderTokens v

-- | Renders 'Parsed' or 'Failed' value as string
renderParsed :: Parsed JValue -> String
renderParsed (Parsed v _) = render v
renderParsed (Failed err) = show err

-- | Parses given file as JSON and renders result
renderJSONFile :: String -> IO String
renderJSONFile file = renderParsed <$> parseJSONFile file

-- | Parses given file as JSON
parseJSONFile :: String -> IO (Parsed JValue)
parseJSONFile file = parse json <$> readFile file

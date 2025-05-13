[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/xY-5QatV)
# Haskell: Applicative parser

<img alt="points bar" align="right" height="36" src="../../blob/badges/.github/badges/points-bar.svg" />

<details>
<summary>Guidelines</summary>

## Guidelines

When solving the homework, strive to create not just code that works, but code that is readable and concise.
Try to write small functions which perform just a single task, and then combine those smaller
pieces to create more complex functions.

Don’t repeat yourself: write one function for each logical task, and reuse functions as necessary.

Don't be afraid to introduce new functions where you see fit.

### Sources

Each task has corresponding source file in [src](src) directory where you should implement the solution.

### Building

All solutions should compile without warnings with following command:

```bash
stack build
```

### Testing

You can and should run automated tests before pushing solution to GitHub via

```bash
stack test --test-arguments "-p TaskX"
```

where `X` in `TaskX` should be number of corresponding Task to be tested.

So to run all test for the first task you should use following command:

```bash
stack test --test-arguments "-p Task1"
```

You can also run tests for all tasks with just

```bash
stack test
```

### Debugging

For debugging you should use GHCi via stack:

```bash
stack ghci
```

You can then load your solution for particular task using `:load TaskX` command.

Here is how to load Task1 in GHCi:

```bash
$ stack ghci
ghci> :load Task1
[1 of 1] Compiling Task1 ( .../src/Task1.hs, interpreted )
Ok, one module loaded.
```

> **Note:** if you updated solution, it can be quickly reloaded in the same GHCi session with `:reload` command
> ```bash
> ghci> :reload
> ```

</details>

## Preface

In this assignment you will implement *applicative* parser from scratch
and use it together with custom library of "parser combinators"
to parse a couple of different formats, like [JSON](https://www.json.org).

> It is recommended for tasks to be implemented in order.

This should give you the fundamental understanding of how
industry-standard parser libraries (such as [parsec](https://hackage.haskell.org/package/parsec),
[megaparsec](https://hackage.haskell.org/package/megaparsec) and
[attoparsec](https://hackage.haskell.org/package/attoparsec))
are implemented and how to efficiently use them in real applications.

> To learn more in-depth about how to use these libraries and other more advanced features of Haskell,
> check out tutorial [Parser Combinators in Haskell](https://serokell.io/blog/parser-combinators-in-haskell).

## Task 1 (3 points)

### Parser

In file [src/Parser.hs](src/Parser.hs) you will find the base for the parser:

```haskell
-- | Parser of value of type @a@
newtype Parser a = Parser { runParser :: Input -> Parsed a }
```

In essence parser is a function that processes its *input* string
and either returns parsed value in a more structured format together
with remaining string to parse, or fails with some parsing error,
ideally indicating where the parsing failed and why (the quality of error
reporting directly correlates with ease of use of your parser).

So to keep track of current position and remaining input to parse,
parser needs to receive and return both position and the string that is being parsed:

```haskell
-- | Parser input encapsulating remaining string to be parsed with current position
type Input = Position String

-- | Parsing result of value of type @a@
data Parsed a =
    Parsed a Input           -- ^ Successfully parsed value of type @a@ with remaining input to be parsed
  | Failed [Position Error]  -- ^ Failed to parse value of type @a@ with accumulated list of errors
```

> Note that instead of just single error, `Failed` constructor accepts list of errors,
> each with its own position where the error occurred. This will be useful for situations,
> when parser will have to try several possible *alternatives* and all of them failed to parse.

The data type `Position a` encapsulates annotation of value of type `a` with position:

```haskell
-- | Value annotated with position of parsed input starting from 0
data Position a = Position Int a
```

Finally, `Error` data type represents supported parsing errors:

```haskell
-- | Parsing error
data Error =
    Unexpected Char -- ^ Unexpected character
  | EndOfInput      -- ^ Unexpected end of input
```

> [!TIP]
>
> Feel free to add more error kinds to simplify debugging for yourself!
>
> Automated tests only check the fact of success or failure, not the exact
> errors that are reported (see `parseMaybe` below).

### Runners

Instead of exposing `runParser` and its implementation details to the end user,
it is always a good idea to provide separate public functions that will run
parser on particular string.

The first function that you should implement is `parse`, which
runs given `Parser` on provided string, absolving the user
from wrapping the string into `Input` for `runParser`:

```haskell
parse :: Parser a -> String -> Parsed a
```

The second function to implement is `parseMaybe`, which hides the details
further by discarding errors and the rest of the input to be parsed,
resulting in simple `Maybe` value:

```haskell
parseMaybe :: Parser a -> String -> Maybe a
```

### Applicative

Next you need to implement instances of `Functor`, `Applicative` and `Alternative`
for `Parser`.

> [!NOTE]
>
> When implementing `Alternative` instance (particularly `(<|>)` operator),
> make sure to accumulate errors if both parsers fail.
> It is also useful to deduplicate errors while accumulating them.

### `satisfy`

The most basic building block for more complex parsers is function `satisfy`
(for you to implement) which parses single character if it satisfies given predicate:

```haskell
satisfy :: (Char -> Bool) -> Parser Char
```

**Example:**

```haskell
>>> parse (satisfy (>= 'b')) "foo"
Parsed 'f' (Position 1 "oo")
>>> parse (satisfy (>= 'b')) "bar"
Parsed 'b' (Position 1 "ar")
>>> parse (satisfy (>= 'b')) "abc"
Failed [Position 0 (Unexpected 'a')]
>>> parse (satisfy (>= 'b')) ""
Failed [Position 0 EndOfInput]
```

> [!IMPORTANT]
>
> The constructor for `Parser` and `runParser` are intentionally
> not exposed outside of [src/Parser.hs](src/Parser.hs) to encourage
> you to use only `satisfy` and `Applicative`/`Alternative` instances
> to implement all of the other functions below.

### Parser combinators

Now it is time to implement your own library of *parser combinators*
in [src/ParserCombinators.hs](src/ParserCombinators.hs), where we
have already provided you with a skeleton of usual combinator ideas.
However, you should improve and extend this library as you solve the rest of the tasks.

> [!TIP]
>
> You can discover more useful parser combinators to implement by following links below
>
> - <https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/Control-Applicative-Combinators.html>
> - <https://hackage.haskell.org/package/parsec-3.1.18.0/docs/Text-Parsec-Char.html>

### Integers

The last part of this first task is to implement two integer parsers
in [src/Task1.hs](src/Task1.hs):

- `nat` for natural numbers
  ```haskell
  nat :: Parser Integer
  ```
  **Example:**
  ```haskell
  >>> parse nat "0"
  Parsed 0 (Input 1 "")
  >>> parse nat "123"
  Parsed 123 (Input 3 "")
  >>> parse nat "-123"
  Failed [PosError 0 (Unexpected '-')]
  >>> parse nat "abc"
  Failed [PosError 0 (Unexpected 'a')]
  >>> parse nat "123abc"
  Parsed 123 (Input 3 "abc")
  ```
- `int` for integer numbers
  ```haskell
  int :: Parser Integer
  ```
  **Example:**
  ```haskell
  >>> parse int "0"
  Parsed 0 (Input 1 "")
  >>> parse int "123"
  Parsed 123 (Input 3 "")
  >>> parse int "-123"
  Parsed (-123) (Input 4 "")
  >>> parse int "abc"
  Failed [PosError 0 (Unexpected 'a')]
  >>> parse int "123abc"
  Parsed 123 (Input 3 "abc")
  ```

## Task 2 (3 points)

The second task is to construct parser for dates
in one of three common formats given as [BNF](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form):

```haskell
date ::= dotFormat | hyphenFormat | usFormat

dotFormat ::= day "." month "." year
hyphenFormat ::= day "-" month "-" year
usFormat ::= monthName " " usDay " " year

usDay ::= nonZeroDigit | "1" digit | "2" digit | "30" | "31"
day ::= "0" nonZeroDigit | "1" digit | "2" digit | "30" | "31"
month ::= "0" nonZeroDigit | "10" | "11" | "12"
year ::= number

number ::= digit | number digit
digit ::= "0" | nonZeroDigit
nonZeroDigit ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

monthName ::= "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"
```

Here are examples of valid dates:

- `01.01.2012` (dot format)
- `12.12.2012` (dot format)
- `01-01-2012` (hyphen format)
- `12-12-2012` (hyphen format)
- `Jan 1 2012` (US format)
- `Dec 12 2012` (US format)

Note that because the formats are given as [context-free grammar](https://en.wikipedia.org/wiki/Context-free_grammar),
they cannot distinguish between months that contain different number of days
and detect leap years (that is not the job of parser after all).

So the following dates are also considered valid by grammar:

- `Feb 31 2012`
- `01.01.00000000`

### Date

In [src/Task2.hs](src/Task2.hs) you will find following data type `Date`
to represent dates as well as `newtype` wrappers for each date component:

```haskell
data Date = Date Day Month Year
  deriving (Show, Eq)

newtype Day   = Day   Int deriving (Show, Eq)
newtype Month = Month Int deriving (Show, Eq)
newtype Year  = Year  Int deriving (Show, Eq)
```

### Parsing

Your task is to implement the following parser `date`:

```haskell
date :: Parser Date
```

**Example:**

```haskell
>>> parse date "01.01.2012"
Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
>>> parse date "12.12.2012"
Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
>>> parse date "12-12-2012"
Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
>>> parse date "Dec 12 2012"
Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 11 "")
>>> parse date "Jan 1 2012"
Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
>>> parse date "Feb 31 2012"
Parsed (Date (Day 31) (Month 2) (Year 2012)) (Input 11 "")
>>> parse date "12/12/2012"
Failed [PosError 2 (Unexpected '/'),PosError 0 (Unexpected '1')]
```

## Task 3 (4 points)

The last task is to construct parser for [JSON](https://www.json.org) format.

> [!IMPORTANT]
>
> You will need to implement the full JSON grammar as it is described at <https://www.json.org>.
>
> Pay close attention to whitespace handling, number format (with fraction and exponent) and
> escape sequences.

### JSON

In [src/Task3.hs](src/Task3.hs) you will find following data type `JValue`
to represent JSON values:

```haskell
data JValue =
    JObject [(String, JValue)]
  | JArray [JValue]
  | JString String
  | JNumber Double
  | JBool Bool
  | JNull
 deriving (Show, Eq)
```

And in directory [samples](samples) you can find some JSON samples
which you can use to test your implementation, as well as their
*expected* rendered format in `.expected` files
(see section [Rendering](#rendering) below).

### Parsing

Your task is to implement the following parser `json`:

```haskell
json :: Parser JValue
```

**Example:**

```haskell
>>> parse json "{}"
Parsed (JObject []) (Input 2 "")
>>> parse json "null"
Parsed JNull (Input 4 "")
>>> parse json "true"
Parsed (JBool True) (Input 4 "")
>>> parse json "3.14"
Parsed (JNumber 3.14) (Input 4 "")
>>> parse json "{{}}"
Failed [PosError 0 (Unexpected '{'),PosError 1 (Unexpected '{')]
```

### Rendering

Also in [src/Task3.hs](src/Task3.hs) we provided a set of rendering utilities,
which should help you debug and verify correctness of your parser.

For example, to parse file [samples/wiki.json](samples/wiki.json) using your
parser into `Parsed JValue`, you can run following command in `stack ghci`:

```haskell
>>> parseJSONFile "samples/wiki.json"
Parsed (JObject [("first_name",JString "John"),("last_name",JString "Smith"),...) (Position 460 "")
```

Or to get a nicely rendered representation which can be directly compared with
[samples/wiki.json.expected](samples/wiki.json.expected):

```haskell
>>> renderJSONFile "samples/wiki.json" >>= putStrLn
{"first_name": "John", "last_name": "Smith", ...}
```

> All `.expected` files directly correspond to the format in which `render` function
> displays given `JValue`, so you can use it as a reference.

Note that when rendering `JValue` directly in GHCi, you will end up with escaped version
of rendered string. To avoid that, you can pass this string to function `putStrLn` which
will print you unescaped representation:

```haskell
>>> render (JArray [JString "abc"])
"[\"abc\"]"
>>> putStrLn (render (JArray [JString "abc"]))
["abc"]
```

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}

-- | Implementation of a parser-combinator.
module Parser where

import           Control.Applicative
import           Data.Char           (isAlpha, isDigit, isLower, isSpace,
                                      isUpper)
import           Instances           (ParseError (..), ParseResult (..),
                                      Parser (..), readInt)
import Data.List (isPrefixOf)
import qualified Data.Functor
import qualified Control.Monad

-- $setup
-- >>> import Instances (isErrorResult, parse)


-- | -------------------------------------------------
-- | --------------- Core parsers --------------------
-- | -------------------------------------------------

-- | Return a parser that always fails with the given error.
--
-- >>> isErrorResult (parse (failed UnexpectedEof) "abc")
-- True
failed :: ParseError -> Parser a
failed = Parser . const . Error

-- | Produces a parser that always fails with 'UnexpectedChar' using the given
-- character.
unexpectedCharParser :: Char -> Parser a
unexpectedCharParser = Parser . const . Error . UnexpectedChar

-- | Return a parser that succeeds with a character off the input or fails with
-- an error if the input is empty.
--
-- >>> parse char "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse char "")
-- True
char :: Parser Char
char = Parser f
  where
    f ""       = Error UnexpectedEof
    f (x : xs) = Result xs x

-- | Parse numbers as int until non-digit

---- >>> parse int "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse int "")
-- True
--
-- >>> isErrorResult (parse int "a")
-- True
int :: Parser Int
int = Parser f
  where
    -- This is okay because the case statement is small
    f "" = Error UnexpectedEof
    f x = case readInt x of
      Just (v, rest) -> Result rest v
      Nothing        -> Error $ UnexpectedChar (head x)

-- | Write a parser that asserts that there is no remaining input.
--
-- >>> parse eof ""
-- Result >< ()
--
-- >>> isErrorResult (parse eof "abc")
-- True
eof :: Parser ()
eof = Parser f
  where
    f "" = Result "" ()
    f x  = Error $ ExpectedEof x

-- | -------------------------------------------------
-- | --------------- Satisfy parsers -----------------
-- | -------------------------------------------------
-- | All of these parsers use the `satisfy` parser!
-- | Return a parser that produces a character but fails if:
--
--   * the input is empty; or
--
--   * the character does not satisfy the given predicate.
--
-- >>> parse (satisfy isUpper) "Abc"
-- Result >bc< 'A'
--
-- >>> isErrorResult (parse (satisfy isUpper) "abc")
-- True
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = char >>= f'
  where
    -- This is okay because guards are small
    f' c
      | f c = pure c
      | otherwise = unexpectedCharParser c

-- | Return a parser that produces the given character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not equal to the given character.
--
-- >>> parse (is 'c') "c"
-- Result >< 'c'
--
-- >>> isErrorResult (parse (is 'c') "")
-- True
--
-- >>> isErrorResult (parse (is 'c') "b")
-- True
is :: Char -> Parser Char
is = satisfy . (==)

-- | Return a parser that produces any character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is equal to the given character.
--
-- >>> parse (isNot 'c') "b"
-- Result >< 'b'
--
-- >>> isErrorResult (parse (isNot 'c') "")
-- True
--
-- >>> isErrorResult (parse (isNot 'c') "c")
-- True
isNot :: Char -> Parser Char
isNot = satisfy . (/=)

-- | Write a function that parses one of the characters in the given string.
--
-- /Hint/: What does `elem` do? What are its parameters?
--
-- >>> parse (oneof "abc") "bcdef"
-- Result >cdef< 'b'
--
-- >>> isErrorResult (parse (oneof "abc") "def")
-- True
oneof :: String -> Parser Char
oneof = satisfy . flip elem

-- | Write a function that parses any character, but fails if it is in the
-- given string.
--
-- /Hint/: What does `notElem` do? What are its parameters?
--
-- >>> parse (noneof "bcd") "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (noneof "abcd") "abc")
-- True
noneof :: String -> Parser Char
noneof = satisfy . flip notElem

-- | Return a parser that produces a character between '0' and '9' but fails if
--
--   * the input is empty; or
--
--   * the produced character is not a digit.
--
-- /Hint/: Use the 'isDigit' function
digit :: Parser Char
digit = satisfy isDigit

-- | Return a parser that produces a space character but fails if
--
--   * the input is empty; or
--
--   * the produced character is not a space.
--
-- /Hint/: Use the 'isSpace' function
space :: Parser Char
space = satisfy isSpace

-- | Return a parser that produces a lower-case character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not lower-case.
--
-- /Hint/: Use the 'isLower' function
lower :: Parser Char
lower = satisfy isLower

-- | Return a parser that produces an upper-case character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not upper-case.
--
-- /Hint/: Use the 'isUpper' function
upper :: Parser Char
upper = satisfy isUpper

-- | Return a parser that produces an alpha character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not alpha.
--
-- /Hint/: Use the 'isAlpha' function
alpha :: Parser Char
alpha = satisfy isAlpha

-- | Write a parser that will parse zero or more spaces (including newlines)
--
-- /Hint/: Remember the `space` parser!
--
-- >>> parse spaces " abc"
-- Result >abc< " "
--
-- >>> parse spaces "abc"
-- Result >abc< ""
spaces :: Parser String
spaces = many space

-- | Return a parser that produces one or more space characters (consuming
-- until the first non-space) but fails if:
--
--   * the input is empty; or
--
--   * the first produced character is not a space.
--
-- /Hint/: Remember the `space` parser!
--
-- >>> parse spaces1 " abc"
-- Result >abc< " "
--
-- >>> isErrorResult $ parse spaces1 "abc"
-- True
spaces1 :: Parser String
spaces1 = some space


-- | Write a parser that will parse zero or more spaces (not including newlines)
--
-- The possible whitespace characters: \t, \r, \f, \v, and a space character.
--
-- >>> parse inlineSpace " abc"
-- Result >abc< " "
--
-- >>> parse inlineSpace "abc"
-- Result >abc< ""
inlineSpace :: Parser String
inlineSpace = many (oneof "\t\r\f\v ")

-- | Write a function that parses the given string (fails otherwise).
--
-- /Hint/: Use 'is' and 'traverse'.
--
-- >>> parse (string "abc") "abcdef"
-- Result >def< "abc"
--
-- >>> isErrorResult (parse (string "abc") "bcdef")
-- True
string :: String -> Parser String
string = traverse is

-- | -------------------------------------------------
-- | --------------- Token parsers -------------------
-- | -------------------------------------------------

-- | Write a function that applies the given parser, then parses 0 or more
-- spaces, then produces the result of the original parser.
--
-- /Hint/: You can use the Monad instance or Applicatives
--
-- >>> parse (tok (is 'a')) "a bc"
-- Result >bc< 'a'
--
-- >>> parse (tok (is 'a')) "abc"
-- Result >bc< 'a'
tok :: Parser a -> Parser a
tok = (<* spaces)

-- tok p = do
--   r <- p
--   spaces
--   pure r

-- | Write a function that parses the given char followed by 0 or more spaces.
--
-- /Hint/: Remember the `is` parser
--
-- >>> parse (charTok 'a') "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (charTok 'a') "dabc")
-- True
charTok :: Char -> Parser Char
charTok = tok . is

-- | Write a parser that parses a comma ',' followed by 0 or more spaces.
--
-- /Hint/: We just implemented `charTok`
--
-- >>> parse commaTok ",123"
-- Result >123< ','
--
-- >>> isErrorResult( parse commaTok "1,23")
-- True
commaTok :: Parser Char
commaTok = charTok ','

-- | Write a function that parses the given string, followed by 0 or more
-- spaces.
--
-- /Hint/: Remember the `string` parser
--
-- >>> parse (stringTok "abc") "abc  "
-- Result >< "abc"
--
-- >>> isErrorResult (parse (stringTok "abc") "bc  ")
-- True
stringTok :: String -> Parser String
stringTok = tok . string

-- Helper Functions -- 
-- parse p until end succeeds
manyTill :: Parser p -> Parser end -> Parser [p]
manyTill p end = (end Data.Functor.$> []) <|> ((:) <$> p <*> manyTill p end)

-- parse any one character
anyChar :: Parser Char
anyChar = Parser $
  \case
    (x:xs) -> Result xs x
    []     -> Error UnexpectedEof

-- look at input without consuming 
lookAhead :: Parser String
lookAhead = Parser $
  \str -> Result str str

-- parser that always fails
parseFail :: String -> Parser a
parseFail s = Parser $
  \_ -> Error (UnexpectedString s)

-- check if not followed by a string
notFollowedBy :: String -> Parser ()
notFollowedBy s = Parser $ \input ->
  if s `isPrefixOf` input then Error (UnexpectedString s) else Result input ()

-- check if followed by a string
followedBy :: String -> Parser ()
followedBy s = Parser $ \input ->
  if s `isPrefixOf` input then Result input () else Error (UnexpectedString s)

-- stop any special strings
filterSpecialChar :: [String] -> Parser ()
filterSpecialChar lst = do
  s <- lookAhead
  Control.Monad.when (any (`isPrefixOf` s) lst) $ parseFail ""

-- parse a quote string
quoteString :: Parser String
quoteString = is '\"' *> many (isNot '\"') <* is '\"'

-- parse until a new line
tillNewline :: Parser String 
tillNewline = many (isNot '\n')

-- at least 1 inline space
inlineSpace1 :: Parser String 
inlineSpace1 = some (oneof "\t\r\f\v ")

-- parse as many characters as possible until an inline space
manyTillInlineSpace :: Parser String 
manyTillInlineSpace = many (noneof "\t\r\f\v ")

-- fail the parser if there's any starting spaces 
failIfSpaces :: Parser ()
failIfSpaces = do
  -- Check if there are any spaces at the current position
  result <- optional (some (oneof "\t\r\f\v\n "))
  case result of
    Just _  -> parseFail "Unexpected spaces found"
    Nothing -> pure ()

-- check if a char is inline space
isInlineSpace :: Char -> Bool
isInlineSpace c = c `elem` "\t\r\f\v "

-- remove trailing spaces
rstrip :: String -> String 
rstrip = reverse . dropWhile isInlineSpace . reverse 

-- remove start and trailing spaces
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- check end of line 
endOfLine :: Parser String 
endOfLine = inlineSpace <* (followedBy "\n" <|> eof)

-- set the input 
setInput :: String -> Parser ()
setInput newInput = Parser $ \_ -> Result newInput ()

-- parse a strictly positive integer
posInt :: Parser Int 
posInt = Parser f
  where 
    f "" = Error UnexpectedEof
    f x = case readInt x of 
      Just (val, rest)
        | val > 0 -> Result rest val 
        | otherwise -> Error UnexpectedEof
      Nothing -> Error $ UnexpectedChar (head x)
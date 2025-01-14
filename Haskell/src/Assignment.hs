module Assignment (markdownParser, convertADTHTML, getTime) where

import Data.Time.Clock  (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Instances (Parser (..), ParseResult (Result, Error))
import Parser (is, isNot, string, anyChar, lookAhead, parseFail, notFollowedBy,
               filterSpecialChar, quoteString, tillNewline, manyTill, followedBy,
               inlineSpace, inlineSpace1, manyTillInlineSpace, rstrip, noneof,
               failIfSpaces, endOfLine, strip, setInput, posInt)
import Control.Applicative (Alternative(..), optional)
import qualified Data.Functor
import Data.Maybe (fromMaybe)

data ADT
  -- Outter Wrapper
  = Newline ADT ADT
  | Start ADT ADT
  -- Text ADT Wrapper
  | HTML ADT ADT
  -- Text Modifiers
  | Text String
  | Bold ADT
  | Italic ADT
  | Strikethrough ADT
  | InlineCode ADT
  | Link ADT String
  | Footnote Int
  -- Footnote Reference
  | FootnoteRef Int String
  -- Image 
  | Image String String String
  -- Heading 
  | Heading Int ADT
  -- Blockquote 
  | Blockquote ADT ADT
  | SubBlockquote ADT ADT
  -- Code Block
  | Code String String
  -- Ordered List
  | OrderedList [(ADT, ADT)]
  | UnorderedList [(ADT, ADT)]
  | NextList [(ADT, ADT)]
  -- Table 
  | Table ADT ADT
  | TableHeading ADT
  | TableRow ADT ADT
  | TableRowCell ADT ADT
  | TableHeadingCell ADT ADT
  -- Empty 
  | Empty
  deriving (Show, Eq)

-- Main parser to parse everything 
markdownParser :: Parser ADT
markdownParser = do
  str <- lookAhead
  let stripped = strip str
  setInput stripped -- replace the trimmed string
  markdownHelper

-- Helper function to parse everything 
markdownHelper :: Parser ADT
markdownHelper = do
  -- try to parse non-text elements first
  tryParser <- optional nonTextModifierParser
  -- parse as normal texts if special parsers fail 
  content <- case tryParser of
      Just content -> return content
      Nothing -> parseNested "" <$> tillNewline
  -- parse the rest starting from a new line 
  restText <- many $ is '\n' *> markdownHelper
  return $ Start content (foldr Newline Empty restText)

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime

-- Convert ADT to HTML 
convertADTHTML :: ADT -> String
convertADTHTML adt = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>"
                     ++ "\n</head>\n\n<body>\n"
                     ++ convertToHTML adt 0 -- helper function 
                     ++ "\n</body>\n\n</html>"

-- Convert to HTML helper function -- 
convertToHTML :: ADT -> Int -> String
convertToHTML (Newline content rest) indent = "\n" ++ convertToHTML content indent ++ convertToHTML rest indent
convertToHTML (Start content rest) indent =
  let contentHTML = convertToHTML content (indent + 1)
    in if isHTML content -- Check if the content is of type HTML
       then indentLine ("<p>" ++ contentHTML ++ "</p>") (indent + 1) ++ convertToHTML rest indent
       else indentLine contentHTML (indent + 1) ++ convertToHTML rest indent
convertToHTML (HTML content rest) indent = convertToHTML content indent ++ convertToHTML rest indent
-- Inline Modifiers
convertToHTML (Text str) _ = str
convertToHTML (Bold content) indent = "<strong>" ++ convertToHTML content indent ++ "</strong>"
convertToHTML (Italic content) indent = "<em>" ++ convertToHTML content indent ++ "</em>"
convertToHTML (Strikethrough content) indent = "<del>" ++ convertToHTML content indent ++ "</del>"
convertToHTML (InlineCode content) indent = "<code>" ++ convertToHTML content indent ++ "</code>"
convertToHTML (Link ref url) indent = "<a href=\"" ++ url ++ "\">" ++ convertToHTML ref indent ++ "</a>"
convertToHTML (Footnote lvl) _ = "<sup><a id=\"fn" ++ show lvl ++ "ref\" href=\"#fn" ++ show lvl ++ "\">" ++ show lvl ++ "</a></sup>"
-- Footnote Reference 
convertToHTML (FootnoteRef lvl ref) _ = "<p id=\"fn" ++ show lvl ++ "\">" ++ ref ++ "</p>"
-- Image 
convertToHTML (Image altText url caption) _ = "<img src=\"" ++ url ++ "\" alt=\"" ++ altText ++ "\" title=\"" ++ caption ++ "\">"
-- Heading 
convertToHTML (Heading lvl heading) indent = "<h" ++ show lvl ++ ">" ++ convertToHTML heading indent ++ "</h" ++ show lvl ++ ">"
-- Blockquotes
convertToHTML (Blockquote content nextBlock) indent = "<blockquote>\n" ++ indentLine ("<p>" ++ convertToHTML content indent ++ "</p>") (indent+1)
                                                                       ++ "\n" ++ convertToHTML nextBlock (indent+1) ++ "    </blockquote>"
convertToHTML (SubBlockquote content nextSubBlock) indent = indentLine ("<p>" ++ convertToHTML content indent ++ "</p>") indent ++ "\n" ++ convertToHTML nextSubBlock indent
-- Code Blocks
convertToHTML (Code language code) _ = "<pre><code class=\"language-" ++ language ++ "\">" ++ code ++ "\n    </code></pre>"
-- List
convertToHTML (OrderedList []) _ = ""
convertToHTML (OrderedList (x:xs)) indent = "<ol>" ++ convertPair (indent+1) x ++  convertToHTML (NextList xs) (indent+1) ++ "\n" ++ replicate (indent*4) ' ' ++ "</ol>"
convertToHTML (UnorderedList []) _ = ""
convertToHTML (UnorderedList (x:xs)) indent = "<ul>" ++ convertPair (indent+1) x ++  convertToHTML (NextList xs) (indent+1) ++ "\n" ++ replicate (indent*4) ' ' ++ "</ul>"
convertToHTML (NextList []) _ = ""
convertToHTML (NextList (x:xs)) indent = convertPair indent x ++ convertToHTML (NextList xs) indent
-- Table
convertToHTML (Table tableHeading tableRow) indent = "<table>\n" ++ convertToHTML tableHeading (indent+1) ++ convertToHTML tableRow (indent+1) ++ "\n    </table>"
convertToHTML (TableHeading tableCell) indent = "        <tr>\n" ++ indentLine (convertToHTML tableCell indent) indent ++ "</tr>"
convertToHTML (TableHeadingCell content nextCell) indent = "    <th>" ++ convertToHTML content indent ++ "</th>" ++ "\n" ++ indentLine (convertToHTML nextCell indent) indent
convertToHTML (TableRow tableCell nextRow) indent = "\n        <tr>" ++ indentLine (convertToHTML tableCell indent) indent ++ "\n        </tr>" ++ convertToHTML nextRow indent
convertToHTML (TableRowCell content nextCell) indent = "\n            <td>" ++ convertToHTML content indent ++ "</td>" ++ convertToHTML nextCell indent
convertToHTML Empty _ = ""

-- Check if inner wrapper is HTML
isHTML :: ADT -> Bool
isHTML (HTML _ _) = True
isHTML _ = False

-- Convert sublists to HTML helper function  
convertPair :: Int -> (ADT, ADT) -> String
convertPair indent (content, sublist) =
  let contentHTML = convertToHTML content (indent+1)
      sublistHTML = convertToHTML sublist (indent+1)
      in "\n" ++ replicate (indent*4) ' ' ++
        if sublistHTML /= []  -- Check if sublist is not empty
          then "<li>" ++ contentHTML ++ "\n" ++ replicate ((indent+1) * 4) ' ' ++ sublistHTML ++ "\n" ++ replicate (indent * 4) ' ' ++ "</li>" -- Add indent before closing </li>
          else "<li>" ++ contentHTML ++ sublistHTML ++ "</li>"

-- Function to indent HTML lines
indentLine :: String -> Int -> String
indentLine line indent = replicate (indent * 4) ' ' ++ line

-- Combined non-text-modifier Parsers -- 
nonTextModifierParser :: Parser ADT
nonTextModifierParser = parseTable <|> parseOrderedList <|> parseUnorderedList <|> parseCode <|> parseBlockquote <|> parseHeading <|> parseImage <|> parseFootnoteRef

-- Text Modifier Parser --
textModifierParser :: String -> Parser ADT
textModifierParser exclude = do
  nested <- many (parseFootnote
              <|> parseLink
              -- prevent the same parser from parsing twice
              <|> (if exclude /= "`" then parseInlineCode else empty)
              <|> (if exclude /= "_" then parseItalic else empty)
              <|> (if exclude /= "**" then parseBold else empty)
              <|> (if exclude /= "~~" then parseStrike else empty)
              <|> parseText
              <|> parseAll)
  return $ HTML (foldr HTML Empty nested) Empty

-- Extension: Helper functions for parsing nested Text Modifiers -- 
-- Parse texts with nested modifiers
parseNested :: String -> String -> ADT
parseNested exclude s = case parse (textModifierParser exclude) s of
  -- try to parse the content using text modifiers
  Result _ res -> res
  -- if fail, treat s as plain texts 
  Error _ -> Text s

-- Helper function to deal with consecutive modifier characters 
parseTextExcluding :: Char -> Int -> Parser [Char]
parseTextExcluding c i = Parser $ \input ->
  let parseChar = some $ do
        str <- lookAhead
        -- tries to parse a character c exactly i times
        if length (takeWhile (== c) str) == i then parseFail "" else anyChar
  in case parse parseChar input of
    Result r1 c1 -> Result (drop i r1) c1
    Error e -> Error e

-- Parse the modifiers
parseModifier :: String -> Int -> Parser ADT
parseModifier s n = do
  -- parse the start modifier
  str <- string s
  -- ensures empty modifiers will be treated as texts
  emptyMod <- string s <|> pure ""
  -- get all texts without the modifier
  texts <- many (notFollowedBy s *> anyChar)
  -- parse as many non-modifier as possible 
  nested <- ((emptyMod ++ texts) ++) <$> parseTextExcluding (last str) n <|> (string s Data.Functor.$> emptyMod ++ texts)

  return $ parseNested s nested

-- Text Parser -- 
parseText :: Parser ADT
parseText = Text <$> some (filterSpecialChar ["_", "**", "~~", "`", "[", "("] *> anyChar)

-- Parse any one character --
parseAll :: Parser ADT
parseAll = Text . pure <$> anyChar

-- Bold Parser --
parseBold :: Parser ADT
parseBold = Bold <$> parseModifier "**" 2

-- Italic Parser --
parseItalic :: Parser ADT
parseItalic = Italic <$> parseModifier "_" 1

-- Strikethrough Parser --
parseStrike :: Parser ADT
parseStrike = Strikethrough <$> parseModifier "~~" 2

-- Inline Code Parser --
parseInlineCode :: Parser ADT
parseInlineCode = InlineCode <$> parseModifier "`" 1

-- Link Parser -- 
parseLink :: Parser ADT
parseLink = do
  text <- is '[' *> many (isNot ']') <* is ']' <* inlineSpace
  url <- is '(' *> inlineSpace *> many (noneof "\t\r\f\v )") <* inlineSpace <* is ')'
  return $ Link (parseNested "" text) url

-- Footnote Parser -- 
parseFootnote :: Parser ADT
parseFootnote = Footnote <$> (is '[' *> is '^' *> posInt <* is ']')

-- Footnote Reference Parser -- 
parseFootnoteRef :: Parser ADT
parseFootnoteRef = do
  lvl <- inlineSpace *> (is '[' *> is '^' *> posInt <* is ']') <* is ':' <* inlineSpace
  FootnoteRef lvl <$> tillNewline

-- Image Parser -- 
parseImage :: Parser ADT
parseImage = do
  altText <- inlineSpace *> is '!' *> is '[' *> many (isNot ']') <* (is ']' *> inlineSpace)
  url <- (is '(' *> inlineSpace) *> manyTillInlineSpace
  caption <- inlineSpace1 *> quoteString <* inlineSpace <* is ')' <* endOfLine
  return $ Image altText url caption

-- Heading Parser -- 
parseHeading :: Parser ADT
parseHeading = parseHashHeading <|> parseLineHeading

parseHashHeading :: Parser ADT
parseHashHeading = do
  hashes <- inlineSpace *> many (is '#')
  heading <- inlineSpace1 *> tillNewline
  -- verify the length
  if length hashes > 6 || null hashes
    then parseFail "Invalid hash heading"
    else return $ Heading (length hashes) (parseNested "" heading)

parseLineHeading :: Parser ADT
parseLineHeading = do
  heading <- tillNewline
  lvl <- is '\n' *> inlineSpace *> (((is '=' <* some (is '=')) Data.Functor.$> 1)
                                <|> ((is '-' <* some (is '-')) Data.Functor.$> 2)) <* endOfLine
  -- verify level
  if lvl `elem` [1, 2]
    then return $ Heading lvl (parseNested "" heading)
    else parseFail "Invalid alternative heading"

-- Blockquote Parser -- 
parseBlockquote :: Parser ADT
parseBlockquote = do
  _ <- inlineSpace *> is '>' *> inlineSpace
  firstLine <- tillNewline
  restLines <- many $ do
                _ <- is '\n' *> inlineSpace *> is '>' *> inlineSpace
                parseNested "" <$> tillNewline
  return $ Blockquote (parseNested "" firstLine) (foldr SubBlockquote Empty restLines) -- SubBlockquote for easier html conversion

-- Code Parser -- 
parseCode :: Parser ADT
parseCode = do
  _ <- inlineSpace *> string "```" *> inlineSpace
  line <- many (isNot '\n') <* is '\n'
  let language = rstrip line -- strip spaces at the end
  code <- (("" Data.Functor.<$ inlineSpace) <* string "```" <* endOfLine) -- case if no code
      <|> manyTill anyChar (is '\n' <* inlineSpace <* string "```" <* endOfLine) -- parse code 
  return $ Code language code

-- Ordered List Parser -- 
parseOrderedList :: Parser ADT
parseOrderedList = do
  firstData <- parseOrderedListItem (Just 1)
  item <- many (is '\n' *> parseOrderedListItem Nothing)
  -- concatenate the items
  let listItems = firstData : item
  return $ OrderedList listItems

parseOrderedSublist :: Int -> Maybe Int -> Parser (ADT, ADT)
parseOrderedSublist indent num = do
  -- look at the input without consuming
  input <- lookAhead
  if take (indent+1) input == ('\n' : replicate indent ' ')
    then do
      -- parse the correct number of spaces
      _ <- is '\n' *> string (replicate indent ' ')
      content <- case num of
        Just n -> string (show n ++ ".") *> inlineSpace1 *> tillNewline
        Nothing -> failIfSpaces *> posInt *> is '.' *> inlineSpace1 *> tillNewline

      -- check for nested sublists
      nestedSublist <- many (parseOrderedSublist ((+) 4 indent) (Just 1))
      return (parseNested "" content, if null nestedSublist then Empty else OrderedList nestedSublist)
    else parseFail "No Sublists!"

parseOrderedListItem :: Maybe Int -> Parser (ADT, ADT)
parseOrderedListItem num = do
  content <- case num of
    Just n -> string (show n ++ ".") *> inlineSpace1 *> tillNewline
    Nothing -> failIfSpaces *> posInt *> is '.' *> inlineSpace1 *> tillNewline

  firstSublist <-  parseOrderedSublist 4 (Just 1) <|> pure (Empty, Empty)
  restSublists <- if firstSublist /= (Empty, Empty) then many (parseOrderedSublist 4 Nothing) else pure []
  -- combine sublists
  let sublist = if firstSublist == (Empty, Empty) then restSublists else firstSublist : restSublists
  return (parseNested "" content, if null sublist then Empty else OrderedList sublist)

-- Extension: Unordered List Parser -- 
parseUnorderedList :: Parser ADT
parseUnorderedList = do
  firstData <- parseUnorderedListItem
  item <- many (is '\n' *> parseUnorderedListItem)
  let listItems = firstData : item
  return $ UnorderedList listItems

parseUnorderedSublist :: Int -> Parser (ADT, ADT)
parseUnorderedSublist indent = do
  input <- lookAhead
  if take (indent+1) input == ('\n' : replicate indent ' ')
    then do
      _ <- is '\n' *> string (replicate indent ' ')
      content <- failIfSpaces *> is '-' *> inlineSpace1 *> tillNewline
      nestedSublist <- many (parseUnorderedSublist ((+) 4 indent))
      return (parseNested "" content, if null nestedSublist then Empty else UnorderedList nestedSublist)
    else parseFail "No Sublists!"

parseUnorderedListItem :: Parser (ADT, ADT)
parseUnorderedListItem = do
  content <- failIfSpaces *> is '-' *> inlineSpace1 *> tillNewline
  firstSublist <-  parseUnorderedSublist 4 <|> pure (Empty, Empty)
  restSublists <- if firstSublist /= (Empty, Empty) then many (parseUnorderedSublist 4) else pure []
  let sublist = if firstSublist == (Empty, Empty) then restSublists else firstSublist : restSublists
  return (parseNested "" content, if null sublist then Empty else UnorderedList sublist)

-- Table Parser -- 
parseTable :: Parser ADT
parseTable = do
  (heading, col_count) <- parseTableHeading
  rest <- some $ do
            _ <- is '\n' <* inlineSpace <* is '|'
            nextRow <- parseRowTableCell
            -- make sure table heading length = dash length 
            if lengthTableCell nextRow /= col_count
              then parseFail "Wrong table format"
              else return nextRow
  return $ Table heading (foldr TableRow Empty rest)

parseTableHeading :: Parser (ADT, Int)
parseTableHeading = do
  -- parse the heading content  
  cell <- inlineSpace *> is '|' *> inlineSpace *> parseHeadingTableCell <* inlineSpace
  -- parse the dashes 
  dashes <- is '\n' *> inlineSpace *> is '|' *> parseDashes
  -- decide if format is right
  if lengthTableCell cell /= lengthTableCell dashes
    then parseFail "Wrong table format"
    else return (TableHeading cell , lengthTableCell cell)

parseDashes :: Parser ADT
parseDashes = do
  dash1 <- inlineSpace *> string "---" <* many (is '-') <* inlineSpace <* is '|'
  restDashes <- optional parseDashes <* endOfLine
  return $ TableRowCell (Text dash1) (fromMaybe Empty restDashes)

-- Gneralised function to parse table cells (Heading or Row)
parseTableCell :: (ADT -> ADT -> ADT) -> Parser ADT
parseTableCell cellConstructor = do
  rawContent <- inlineSpace *> many (noneof "|\n") <* is '|'
  let content = rstrip rawContent
  restContent <- optional (parseTableCell cellConstructor) <* endOfLine
  return $ cellConstructor (parseNested "" content) (fromMaybe Empty restContent)

-- Wrapper for parsing a row table cell
parseRowTableCell :: Parser ADT
parseRowTableCell = parseTableCell TableRowCell

-- Wrapper for parsing a heading table cell
parseHeadingTableCell :: Parser ADT
parseHeadingTableCell = parseTableCell TableHeadingCell

-- Find number of columns in a row
lengthTableCell :: ADT -> Int
lengthTableCell Empty = 0
lengthTableCell (TableHeadingCell _ rest) = 1 + lengthTableCell rest
lengthTableCell (TableRowCell _ rest) = 1 + lengthTableCell rest
lengthTableCell _ = 0  -- This handles non-TableCell cases
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import Assignment (markdownParser)

import           Assignment              (convertADTHTML, markdownParser, getTime)
import           Data.Aeson              (object, (.=))
import           Data.Aeson.Key          (fromString)
import           Data.Text.Lazy          (Text, pack, unpack)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Instances               (ParseResult (Result, Error), parse, Parser, many)
import           Web.Scotty              (ActionM, body, json, post, scotty)
import           Parser                  (is, isNot, quoteString)
import           Control.Monad.Cont      (MonadIO(liftIO))
import           qualified Data.Text as T

getResult :: ParseResult a -> (a -> String) -> String
getResult (Result _ a) f = f a
getResult _ _            = ""

-- Magic code to convert key, value pairs to JSON to send back to the server
jsonResponse :: [(String, String)] -> ActionM ()
jsonResponse pairs =
  json $ object [fromString key .= (pack value :: Text) | (key, value) <- pairs]

-- helper function to parse the json string 
parseJSON :: Parser (String, String)  -- Return a tuple of (content, title)
parseJSON = do
  _ <- is '{' *> quoteString *> is ':'
  texts <- many (isNot ',') <* is ','
  let content = replaceEscapedNewlines texts
  title <- quoteString *> is ':' *> many (isNot '<')
  return (content, title)

-- replace escaped new line characters with a new line
replaceEscapedNewlines :: String -> String
replaceEscapedNewlines [] = []
replaceEscapedNewlines ('\\':'n':xs) = '\n' : replaceEscapedNewlines xs
replaceEscapedNewlines (x:xs) = x : replaceEscapedNewlines xs

main :: IO ()
main = scotty 3000 $ do
  post "/api/convertMD" $ do
    requestBody <- body
    let requestBodyText = decodeUtf8 requestBody
        str = unpack requestBodyText
        converted_html = getResult (parse markdownParser str) convertADTHTML
        jsonResult = parse parseJSON str

    -- check if saving format is right
    case jsonResult of
      Result _ (content, title) -> do
        -- get file name 
        formatTime <- liftIO getTime
        -- replace title
        let updatedHtml = if title /= ""
                          then T.unpack (T.replace (T.pack "<title>Test</title>") (T.pack ("<title>" ++ title ++ "</title>")) (T.pack content))
                          else T.unpack (T.replace (T.pack "<title>Test</title>") (T.pack "<title>Converted HTML</title>") (T.pack content))
            fileName = formatTime ++ ".html"

        -- save file 
        liftIO $ writeFile fileName updatedHtml

      Error _ -> jsonResponse [("html", converted_html)]
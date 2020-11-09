module Parser where
import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import Data.Char

data Json = JsonNull
          | JsonBool Bool
          | JsonNumber Int
          | JsonString String
          | JsonArray [Json]
          | JsonObject (M.Map String Json)
          deriving (Show)

spacesOnly :: Parser ()
spacesOnly = skipMany $ oneOf " \t"

parseNull :: Parser Json
parseNull = string "null" >>= (const . return $ JsonNull)

parseNumber :: Parser Json
parseNumber = do
  number <- JsonNumber . read <$> many1 digit
  return number

parseBool :: Parser Json
parseBool = do
  bool <- string "true" <|> string "false"
  case bool of
    "true" -> return $ JsonBool True
    "false" -> return $ JsonBool False

parseString :: Parser Json
parseString = do
  char '"'
  str <- many (noneOf "\"")
  char '"'
  return $ JsonString str

parseJsonArray :: Parser Json
parseJsonArray = do
  char '['
  spaces
  values <- sepBy (spaces >> parseJsonValue) (char ',') 
  spaces
  char ']'
  return $ JsonArray values

parseJsonObject :: Parser Json
parseJsonObject = do
  char '{'
  spaces
  item <- sepBy (spaces >> parseJsonItem) (char ',')
  spaces
  char '}'
  return $ JsonObject $ M.fromList item
    where parseJsonItem :: Parser (String, Json)
          parseJsonItem = do
            spaces
            char '"'
            key <- many (noneOf "\"")
            char '"'
            char ':'
            value <- parseJsonValue
            return (key, value)
  

parseJsonValue :: Parser Json
parseJsonValue =  spacesOnly 
               >> (parseNull
              <|> parseNumber
              <|> parseBool
              <|> parseString
              <|> parseJsonArray
              <|> parseJsonObject)

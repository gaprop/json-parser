module Main where
import Parser
import Text.ParserCombinators.Parsec
import System.Environment
import System.IO

main = do
  args   <- getArgs
  withFile (args !! 0) ReadMode (\handle -> do 
    content <- hGetContents handle
    case parse parseJsonValue "Json parser" content of
      Right parsed -> print parsed
      Left  err    -> print err)

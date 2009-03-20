#!/home/dss/conundrum/bin/runhaskell
import System.Cmd
import System.Environment
import Text.ParserCombinators.Parsec
import Text.Printf
import Data.List

main :: IO ()
main = do 
  as <- getArgs
  let filename = head as
  fileContent <- readFile filename
  case parseQC fileContent of
    Left msg -> putStrLn $ show msg
    Right lns -> putStrLn $ reportErrors lns 
  
-- TBF: should the parser be doing this work?

reportErrors :: [[String]] -> String
reportErrors lines = if errors == "" then "No Failing Quick Check Properties" else (show . length $ elemIndices '\n' errors) ++ " Errors:\n" ++ errors
  where
    errors = concat . reportErrors' $ lines

reportErrors' :: [[String]] -> [String]
reportErrors' [] = []
reportErrors' (x:xs) = (reportError x) :(reportErrors' xs)

reportError :: [String] -> String
reportError line | length line == 2 && isInfixOf "prop_" (head line) = checkForError line
                 | otherwise = ""

checkForError :: [String] -> String
checkForError line | isInfixOf "OK" (last line) = printf "Error in %s : %s\n" (head line) (last line)
                   | otherwise = ""

parseQC :: String -> Either ParseError [[String]]
parseQC input = parse qcFile "(unknown)" input

qcFile = endBy line eol 
line = sepBy cell (char ',') 
eol = char '\n'  
cell = many (noneOf ",\n")



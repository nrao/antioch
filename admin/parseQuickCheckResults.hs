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
    Right lns -> putStrLn $ (reportErrors lns) ++ "\n" ++  (reportTests lns) 
  
-- TBF: should the parser be doing this work?

reportErrors :: [[String]] -> String
reportErrors lines = if errors == "" then "No Failing Quick Check Properties" else (show . length $ elemIndices '\n' errors) ++ " Failing Quick Check Properties:\n" ++ errors
  where
    errors = concat . reportErrors' $ lines

reportTests :: [[String]] -> String
reportTests lines = printf "%d Quick Check Properties: %s\n" numTests tests
  where
    tests = concat . reportTests' $ lines
    numTests = length $ elemIndices '\n' tests
    

reportTests' :: [[String]] -> [String]
reportTests' [] = []
reportTests' (x:xs) = (reportTest x) :(reportTests' xs)

reportTest :: [String] -> String
reportTest line | isInfixOf "prop_" (head line) = (head line) ++ "\n"
                 | otherwise = ""

reportErrors' :: [[String]] -> [String]
reportErrors' [] = []
reportErrors' (x:xs) = (reportError x) :(reportErrors' xs)
--reportErrors' lines = foldr reportError [] lines


reportError :: [String] -> String
reportError line | isInfixOf "prop_" (head line) = checkForError line
                 | otherwise = ""

checkForError :: [String] -> String
checkForError line | not $ isInfixOf "OK" (head line) = printf "Error: %s\n" (head line)
                   | otherwise = ""

parseQC :: String -> Either ParseError [[String]]
parseQC input = parse qcFile "(unknown)" input

qcFile = endBy line eol 
line = sepBy cell (char ',') 
eol = char '\n'  
cell = many (noneOf "\n")



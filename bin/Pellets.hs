import Data.List (isPrefixOf)
import Syntax.Parser (stmtFromStr)
import Syntax.Pretty (prettyStmt)
import System.IO

main :: IO ()
main = do
  putStrLn "Pellets REPL :)"
  repl

repl :: IO ()
repl = do
  input <- promptUser
  if isREPLCommand input
    then return ()
    else do
      processStmt input
      repl

prompt :: String
prompt = "hoot> "

promptUser :: IO String
promptUser = putStr prompt >> hFlush stdout >> getLine

processStmt :: String -> IO ()
processStmt inp = do
  putStr prompt
  case stmtFromStr inp of
    Left e -> putStrLn "Error!!" >> (putStrLn $ show e)
    Right st -> putStrLn "Got: " >> (putStrLn $ prettyStmt 0 st)

isREPLCommand :: String -> Bool
isREPLCommand = isPrefixOf "::"

import Data.List (isPrefixOf)
import Syntax.Parser (stmtFromStr)
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

promptUser :: IO String
promptUser = putStr "hoot> " >> hFlush stdout >> getLine

processStmt :: String -> IO ()
processStmt inp = do
  case stmtFromStr inp of
    Left e   -> putStrLn $ ">>=err=> " ++ e
    Right st -> putStr ">>=got=> " >> print st

isREPLCommand :: String -> Bool
isREPLCommand = isPrefixOf "::"

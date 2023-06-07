import Control.Monad.State
import Data.List (isPrefixOf)
import qualified Eval.Pretty as EP
import qualified Eval.Data as ED
import Syntax.AST
import Syntax.Parser (stmtFromStr)
import Syntax.Pretty (prettyStmt)
import System.IO

main :: IO ()
main = do
  putStrLn "Pellets REPL"
  repl initState
  putStrLn "Exiting Pellets REPL..."

data ReplState = ReplState
  { env :: ED.Environment,
    history :: [String],
    lastResult :: ReplAction
  }
  deriving (Show)

data ReplAction
  = BoundValue ED.EnvScope
  | Evaluated ED.EvalValue
  | Error ReplError
  deriving (Show)

fromEvalResult :: ED.EvalResult -> ReplAction
fromEvalResult (Left ee) = Error $ EvalError ee
fromEvalResult (Right val) = Evaluated val

data ReplError
  = EvalError ED.EvalError
  | ParseError String
  | UnknownCommand String
  deriving (Show)

initState :: ReplState
initState = ReplState [] [] (Evaluated ED.EVUnit)

updateEnv :: ED.EnvScope -> State ReplState ED.EnvScope
updateEnv scp = state $ \(ReplState env h l) -> (scp, ReplState (scp : env) h l)

updateHist :: String -> State ReplState String
updateHist inp = state $ \(ReplState env h l) -> (inp, ReplState env (inp : h) l)

updateLast :: ReplAction -> State ReplState ReplAction
updateLast e@(Error _) = state $ \(ReplState env h l) -> (e, ReplState env h l)
updateLast act = state $ \(ReplState env h l) -> (act, ReplState env h act)

repl :: ReplState -> IO ()
repl st = do
  input <- promptUser
  if isREPLCommand input
    then return ()
    else do
      let (act, st') = runState (processInput input) st
      displayAction act
      repl st'

prompt :: String
prompt = "hoot> "

promptUser :: IO String
promptUser = putStr prompt >> hFlush stdout >> getLine

displayAction :: ReplAction -> IO ()
displayAction (Error err) = putStr "error: " >> print err
displayAction (Evaluated val) = putStr "value: " >> print val
displayAction (BoundValue sc) = putStr "bound: " >> print sc

isREPLCommand :: String -> Bool
isREPLCommand = isPrefixOf "::"

processInput :: String -> State ReplState ReplAction
processInput input = do
  inp <- updateHist input
  case stmtFromStr inp of
    Left e -> state $ \st -> (Error $ ParseError e, st)
    Right st -> processStmt st

-- TODO  this doesn't feel right, there should be a  more elegant way to express this
processStmt :: Statement -> State ReplState ReplAction
processStmt (Expr expr) = do
  st <- get
  let ev = env st
      act = fromEvalResult $ evalExpr ev expr
  updateIt act
  updateLast act
processStmt (Decl decl) = do
  st <- get
  case evalDecl (env st) ED.emptyScope decl of
    Left ee -> state $ \s -> (Error $ EvalError ee, s)
    Right scp -> do
      updateEnv scp
      state $ \s -> (BoundValue scp, s)

updateIt :: ReplAction -> State ReplState ReplAction
updateIt ra@(Evaluated val) = do
  updateEnv sc
  return ra
  where
    sc = mkItScope val
updateIt ra = state $ \s -> (ra, s)

mkItScope :: ED.EvalValue -> ED.EnvScope
mkItScope val = ED.updateScopeEntry ED.emptyScope (Name "it") ent
  where ent = (ED.EnvEntry (Just val) Nothing)

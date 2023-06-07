module Eval.Environment
  ( EvalValue (..),
    EvalError (..),
    EvalResult,
    truthy,
    arithBinop,
    relBinop,
    logicBinop,
    EnvEntry (..),
    EnvScope,
    emptyScope,
    mkEntry,
    combineEntry,
    updateScopeValue,
    updateScopeType,
    expandScope,
    Environment,
    lookupEnv,
  )
where

import qualified Data.Map as M
import Syntax.AST

-- | Type representing the result of a program evaluation
data EvalValue
  = EVUnit
  | EVInt Integer
  | EVBool Bool
  | EVFunction EnvScope Expression
  deriving (Show)

data EvalError
  = EEUndefined Name
  | EETypeError
  deriving (Show)

type EvalResult = Either EvalError EvalValue

truthy :: EvalValue -> Bool
truthy EVUnit = False
truthy (EVInt 0) = False
truthy (EVInt _) = True
truthy (EVBool b) = b
truthy (EVFunction _ _) = True

arithBinop :: (Integer -> Integer -> Integer) -> EvalValue -> EvalValue -> EvalResult
arithBinop f (EVInt rr) (EVInt ll) = Right (EVInt $ f ll rr)
arithBinop _ _ _ = Left EETypeError

relBinop :: (Integer -> Integer -> Bool) -> EvalValue -> EvalValue -> EvalResult
relBinop f (EVInt rr) (EVInt ll) = Right (EVBool $ f ll rr)
relBinop _ _ _ = Left EETypeError

logicBinop :: (Bool -> Bool -> Bool) -> EvalValue -> EvalValue -> EvalResult
logicBinop f (EVBool rr) (EVBool ll) = Right (EVBool $ f ll rr)
logicBinop _ _ _ = Left EETypeError

data EnvEntry
  = Free
  | Entry (Maybe EvalValue) (Maybe Type)
  deriving (Show)

mkEntry :: Maybe EvalValue -> Maybe Type -> EnvEntry
mkEntry val typ = Entry val typ

combineEntry :: EnvEntry -> EnvEntry -> EnvEntry
combineEntry Free Free = Free
combineEntry Free e@(Entry _ _) = e
combineEntry e@(Entry _ _) Free = e
combineEntry (Entry lval ltyp) (Entry rval rtyp) =
  mkEntry val typ
  where
    val = case lval of
      Nothing -> rval
      _ -> lval
    typ = case ltyp of
      Nothing -> rtyp
      _ -> ltyp

type EnvScope = M.Map Name EnvEntry

emptyScope :: M.Map Name EnvEntry
emptyScope = M.empty

expandScope :: EnvScope -> [Name] -> EnvScope
expandScope scope = foldl (\s n -> M.insert n Free s) scope

updateScopeValue :: EnvScope -> Name -> EvalValue -> EnvScope
updateScopeValue scope name val =
  case M.lookup name scope of
    Nothing -> M.insert name entry scope
    Just ent -> M.insert name (combineEntry ent entry) scope
  where
    entry = mkEntry (Just val) Nothing

updateScopeType :: EnvScope -> Name -> Type -> EnvScope
updateScopeType scope name typ =
  case M.lookup name scope of
    Nothing -> M.insert name entry scope
    Just ent -> M.insert name (combineEntry ent entry) scope
  where
    entry = mkEntry Nothing $ Just typ

type Environment = [EnvScope]

lookupEnv :: Environment -> Name -> Maybe EnvEntry
lookupEnv [] nn = Nothing
lookupEnv (sc : scs) nn =
  case M.lookup nn sc of
    Nothing -> lookupEnv scs nn
    x@(_) -> x

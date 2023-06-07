{-# OPTIONS_GHC -Wno-unused-matches #-}
module Eval.Data
  ( EvalValue (..),
    EvalError (..),
    EvalResult,
    EnvEntry (EnvEntry),
    EnvScope,
    emptyScope,
    freeEntry,
    mkBlankScope,
    combineEntry,
    updateBinding,
    updateScopeEntry,
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
  | EVClosure Environment [Name] Expression
  deriving (Show)

type EvalResult = Either EvalError EvalValue

data EvalError
  = EEUndefined Name
  | EETypeError
  deriving (Show)

data EnvEntry = EnvEntry { value :: Maybe EvalValue, type' :: Maybe Type }
  deriving (Show)

freeEntry :: EnvEntry
freeEntry = EnvEntry Nothing Nothing

type Environment = [EnvScope]

combineEntry :: EnvEntry -> EnvEntry -> EnvEntry
combineEntry (EnvEntry lval ltyp) (EnvEntry rval rtyp) =
  EnvEntry (lval >> rval) (ltyp >> rtyp)

type EnvScope = M.Map Name EnvEntry

emptyScope :: M.Map Name EnvEntry
emptyScope = M.empty

mkBlankScope :: [Name] -> EnvScope
mkBlankScope = foldl (\s n -> M.insert n freeEntry s) M.empty

lookupEnv :: Environment -> Name -> Maybe EnvEntry
lookupEnv [] nn = Nothing
lookupEnv (sc : scs) nn =
  case M.lookup nn sc of
    Nothing -> lookupEnv scs nn
    x -> x

-- | Update the environment with the given name and entry pair.
-- | If an environment contains a binding that shadows an existing binding,
-- |   only the shadowing binding is changed. Example :
{-
let x be {
  let y be 10;
  let scope be {
    let y be false;
    y
  };
  scope
};

# x is bound to false
x
-}
-- | If no binding for `Name` exists, this function is idempotent.
updateBinding :: Environment -> Name -> EnvEntry -> Environment
updateBinding [] _ _ = []
updateBinding (sc : scs) nn ent =
  case M.lookup nn sc of
    Nothing -> sc : scs
    Just ee -> let ent' = ent -- ent' = updateEntry ee ent
                   scope = M.insert nn ent' sc
               in scope : scs

updateScopeEntry :: EnvScope -> Name -> EnvEntry -> EnvScope
updateScopeEntry scp name ent = M.insert name ent' scp
  where
    ent' = case M.lookup name scp of
      Just from -> updateEntry from ent
      Nothing -> ent
          

-- | Update an entry with the information with another entry.
updateEntry :: EnvEntry -> EnvEntry -> EnvEntry
updateEntry (EnvEntry (Just fromVal) _) to@(EnvEntry toVal@(Just _) _) = to
-- vv these cases should attempt to type check the value with the new type
-- updateEntry (EnvEntry (Just fromVal) _) (EnvEntry Nothing (Just )) = to
-- updateEntry (EnvEntry Nothing (Just fromTy)) to@(EnvEntry (Just val) Nothing) = ff
updateEntry ff@(EnvEntry (Just fromVal) _) (EnvEntry Nothing (Just _)) = ff
updateEntry (EnvEntry Nothing (Just fromTy)) to@(EnvEntry (Just val) Nothing) = to
updateEntry (EnvEntry Nothing Nothing) to = to
updateEntry from (EnvEntry Nothing Nothing) = from

module Eval (evalExpr, evalDecl) where

import Analysis.Types (typecheck)
import qualified Data.Map as M
import Eval.Environment
import Syntax.AST

-- | Evaluates the given Expression in the given Environment, returning an EvalValue
evalExpr :: Environment -> Expression -> EvalResult
evalExpr env EUnit = Right EVUnit
evalExpr env (EInt i) = Right $ EVInt i
evalExpr env (EBool b) = Right $ EVBool b
evalExpr env (EVar name) =
  case lookupEnv env name of
    Just (Entry (Just vv) _) -> Right vv
    _ -> Left $ EEUndefined name
evalExpr env (EParen expr) = evalExpr env expr
evalExpr env (EBinaryOp op ll rr) = evalBinaryOp env op ll rr
evalExpr env (EUnaryOp op expr) = evalUnaryOp env op expr
evalExpr env (ECond condExp thenExp elseExp) =
  case cond' of
    Left ee -> Left ee
    Right rr -> if truthy rr then then' else else'
  where
    cond' = evalExpr env condExp
    then' = evalExpr env thenExp
    else' = evalExpr env elseExp
evalExpr env (EFunc args body) = Right $ EVFunction scope body
  where
    scope = expandScope M.empty args
evalExpr env (ECall func arg) = Right EVUnit
evalExpr env (ESeq stmts) = Right EVUnit

evalBinaryOp :: Environment -> BinOperator -> Expression -> Expression -> EvalResult
evalBinaryOp env op left right = do
  ll <- evalExpr env left
  rr <- evalExpr env right
  case op of
    BOAdd -> arithBinop (+) ll rr
    BOSub -> arithBinop (-) ll rr
    BOMul -> arithBinop (*) ll rr
    BODiv -> arithBinop div ll rr
    BOEq -> relBinop (==) ll rr
    BONeq -> relBinop (/=) ll rr
    BOLt -> relBinop (<) ll rr
    BOLtEq -> relBinop (<=) ll rr
    BOGt -> relBinop (>) ll rr
    BOGtEq -> relBinop (>=) ll rr
    BOAnd -> logicBinop (&&) ll rr
    BOOr -> logicBinop (||) ll rr

evalUnaryOp :: Environment -> UnOperator -> Expression -> EvalResult
evalUnaryOp env op expr =
  case (op, val) of
    (UONeg, Right (EVInt i)) -> Right $ EVInt (-i)
    (UONot, Right (EVBool b)) -> Right $ EVBool . not $ b
    _ -> Left EETypeError
  where
    val = evalExpr env expr

-- | Given a list of Declarations, return an EvnScope with any new bindings
evalDecs :: Environment -> [Declaration] -> Either EvalError EnvScope
evalDecs env decs = foldl evalDecInEnv base decs
  where
    base = Right M.empty
    evalDecInEnv result decl =
      case result of
        Right scope -> evalDecl env scope decl
        _ -> result

-- | Evaluates a Declaration, returning an EnvScope updated with
-- | new information
-- | Bound expressions are evaluated in an environment containing all its arguments.
-- | The returned scope does not contain those free bindings.
evalDecl :: Environment -> EnvScope -> Declaration -> Either EvalError EnvScope
evalDecl env scope (DValue nn args expr) = do
  let
    scope' = expandScope scope args
    env' = scope' : env
  val <- evalExpr env' expr
  Right $ updateScopeValue scope nn val
evalDecl env scope (DType nn typ') = Right $ updateScopeType scope nn typ'

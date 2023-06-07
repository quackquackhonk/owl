module Eval (evalExpr, evalDecl) where

import Eval.Data
import Eval.Pretty
import Syntax.AST

-- | Evaluates the given Expression in the given Environment, returning an EvalValue
evalExpr :: Environment -> Expression -> EvalResult
evalExpr _ EUnit = Right EVUnit
evalExpr _ (EInt i) = Right $ EVInt i
evalExpr _ (EBool b) = Right $ EVBool b
evalExpr env (EVar name) =
  case lookupEnv env name of
    Just (EnvEntry (Just vv) _) -> Right vv
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
evalExpr env (EFunc args body) = Right $ EVClosure (scope : env) args body
  where
    scope = mkBlankScope args
evalExpr env (ECall func arg) = do
  fval <- evalExpr env func
  aval <- evalExpr env arg
  evalCall fval aval
evalExpr env (ESeq stmts) = evalStmts env emptyScope stmts

evalCall :: EvalValue -> EvalValue -> EvalResult
evalCall (EVClosure _ [] _) _ = Left EETypeError
evalCall (EVClosure env [aa] body) arg =  evalExpr env' body
  where
    env' = updateBinding env aa (EnvEntry (Just arg) Nothing)
evalCall (EVClosure env (aa : args) body) arg = Right $ EVClosure env' args body
  where
    env' = updateBinding env aa (EnvEntry (Just arg) Nothing)
evalCall _ _ = Left EETypeError

-- if no next argument, type error
-- else: eval arg, enter into cEnv
-- if last argument: eval body in cEnv
-- else: return modified closure
evalStmts :: Environment -> EnvScope -> [Statement] -> EvalResult
evalStmts _ _ [] = Right EVUnit
evalStmts env scp [stmt] = res
  where
    (_, res) = evalStmt env scp stmt
evalStmts env scp (stmt : stmts) = evalStmts env scp' stmts
  where
    (scp', _) = evalStmt env scp stmt

evalStmt :: Environment -> EnvScope -> Statement -> (EnvScope, EvalResult)
evalStmt env scp (Decl decl) =
  case evalDecl (scp : env) scp decl of
    Left err -> (scp, Left err)
    Right scp' -> (scp', Right EVUnit)
evalStmt env scp (Expr expr) = (scp, evalExpr (scp : env) expr)

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

truthy :: EvalValue -> Bool
truthy EVUnit = False
truthy (EVInt 0) = False
truthy (EVInt _) = True
truthy (EVBool b) = b
truthy (EVClosure {}) = True

arithBinop :: (Integer -> Integer -> Integer) -> EvalValue -> EvalValue -> EvalResult
arithBinop f (EVInt rr) (EVInt ll) = Right (EVInt $ f ll rr)
arithBinop _ _ _ = Left EETypeError

relBinop :: (Integer -> Integer -> Bool) -> EvalValue -> EvalValue -> EvalResult
relBinop f (EVInt rr) (EVInt ll) = Right (EVBool $ f ll rr)
relBinop _ _ _ = Left EETypeError

logicBinop :: (Bool -> Bool -> Bool) -> EvalValue -> EvalValue -> EvalResult
logicBinop f (EVBool rr) (EVBool ll) = Right (EVBool $ f ll rr)
logicBinop _ _ _ = Left EETypeError

evalUnaryOp :: Environment -> UnOperator -> Expression -> EvalResult
evalUnaryOp env op expr =
  case (op, val) of
    (UONeg, Right (EVInt i)) -> Right $ EVInt (-i)
    (UONot, Right (EVBool b)) -> Right $ EVBool . not $ b
    _ -> Left EETypeError
  where
    val = evalExpr env expr

-- | Evaluates a Declaration, returning an EnvScope updated with
-- | new information
-- | Bound expressions are evaluated in an environment containing all its arguments.
-- | The returned scope does not contain those free bindings.
evalDecl :: Environment -> EnvScope -> Declaration -> Either EvalError EnvScope
evalDecl env scope (DValue nn [] expr) = do
  val <- evalExpr env expr
  Right $ updateScopeEntry scope nn (EnvEntry (Just val) Nothing)
evalDecl env scope (DValue nn args expr) = Right $ updateScopeEntry scope nn (EnvEntry (Just val) Nothing)
  where
    scope' = mkBlankScope args
    env' = scope' : env
    val = EVClosure env' args expr
evalDecl _ scope (DType nn typ') =
  Right $ updateScopeEntry scope nn $ EnvEntry Nothing (Just typ')

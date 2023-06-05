-- Functions for pretty printing OWL ASTs

module Syntax.Pretty
  ( prettyProgram,
    prettyDecl,
    prettyExpr,
    prettyStmt,
    prettyType,
    prettyName,
  )
where

import Syntax.AST

indent :: String
indent = "  "

indentation :: Int -> String
indentation nn = foldl (++) "" $ take nn $ repeat indent

newline :: Int -> String
newline lvl = "\n" ++ indentation lvl

prettyProgram :: Program -> String
prettyProgram (Program decls expr) =
  "Declarations:\n"
    ++ (unlines $ map (prettyDecl 1) decls)
    ++ "Expression:\n"
    ++ prettyExpr 1 expr

prettyDecl :: Int -> Declaration -> String
prettyDecl lvl (DValue nn aa bb) =
  ii ++ pref ++ name ++ args ++ ":\n" ++ body
  where
    pref = case aa of
      [] -> "Val "
      _ -> "Func"
    ii = indentation lvl
    name = prettyName nn
    args = unwords $ map prettyName aa
    body = prettyExpr (lvl + 1) bb
prettyDecl lvl (DType nn tt) =
  ii ++ "Type" ++ name ++ ": " ++ type'
  where
    ii = indentation lvl
    name = prettyName nn
    type' = prettyType tt

indentList :: Int -> [String] -> [String]
indentList lvl [] = []
indentList lvl ( l:ls ) = (ii ++ l) : (indentList lvl ls)
  where
    ii = indentation lvl

prettyExpr :: Int -> Expression -> String
prettyExpr lvl (ESeq es) = "Sequence {\n" ++ exprs ++ ii ++ "}"
  where
    ii = indentation lvl
    exprs = unlines $ indentList (1 + lvl) $ map (prettyStmt (lvl + 1)) es
prettyExpr lvl expr = (indentation lvl) ++ (prettyExpr' expr)
  where
    prettyExpr' EUnit = "()"
    prettyExpr' (EInt ii) = show ii
    prettyExpr' (EBool bb) = show bb
    prettyExpr' (EVar nn) = "<EVar" ++ (prettyName nn) ++ ">"
    prettyExpr' (EParen ee) =
      "(" ++ (prettyExpr' ee) ++ ")"
    prettyExpr' (EBinaryOp op ll rr) =
      prettyExpr' ll ++ prettyBOp op ++ prettyExpr' rr
    prettyExpr' (EUnaryOp op ee) =
      prettyUOp op ++ prettyExpr' ee
    prettyExpr' (ECond cc tt ee) =
      "If {" ++ cond ++ "} Then {" ++ then' ++ "} Else {" ++ else' ++ "}"
      where
        cond = prettyExpr' cc
        then' = prettyExpr' tt
        else' = prettyExpr' ee
    prettyExpr' (EFunc aa bb) =
      "Func [" ++ args ++ "] {" ++ body ++ "}"
      where
        args = unwords $ map prettyName aa
        body = prettyExpr' bb
    prettyExpr' (ECall ff aa) = "Call " ++ func ++ " With " ++ arg
      where
        func = prettyExpr' ff
        arg = prettyExpr' aa
    prettyExpr' xx = prettyExpr lvl xx

prettyStmt :: Int -> Statement -> String
prettyStmt lvl st =
  case st of
    Expr es@(ESeq _) -> prettyExpr lvl es
    Expr ex -> "Expr: " ++ prettyExpr 0 ex
    Decl de -> "Decl: " ++ prettyDecl 0 de
  where
    ii = indentation lvl

prettyType :: Type -> String
prettyType (TVar nn) =
  "<TVar" ++ (prettyName nn) ++ ">"
prettyType TUnit = "TUnit"
prettyType TInt = "TInt"
prettyType TBool = "TBool"
prettyType (TParen pp) =
  "(" ++ (prettyType pp) ++ ")"
prettyType (TArrow ff tt) = from ++ " -> " ++ to
  where
    from = prettyType ff
    to = prettyType tt

prettyName :: Name -> String
prettyName (Name nn) = "`" ++ nn ++ "`"

prettyBOp :: BinOperator -> String
prettyBOp op = case op of
  BOAdd -> "+"
  BOSub -> "-"
  BOMul -> "*"
  BODiv -> "/"
  BOEq -> "="
  BONeq -> "!="
  BOLt -> "<"
  BOLtEq -> "<="
  BOGt -> ">"
  BOGtEq -> ">="
  BOAnd -> "&"
  BOOr -> "|"

prettyUOp :: UnOperator -> String
prettyUOp op = case op of
  UONeg -> "-"
  UONot -> "!"

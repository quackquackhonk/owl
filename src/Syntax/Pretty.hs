-- Functions for pretty printing OWL ASTs

module Syntax.Pretty where

import Syntax.AST

indent :: String
indent = "  "

indentation :: Int -> String
indentation nn = foldl (++) "" $ take nn $ repeat indent

prettyProgram :: Program -> String
prettyProgram (Program decls expr) =
  "Declarations:\n"
    ++ (unlines $ map (prettyDecl 1) decls)
    ++ "Expression:\n"
    ++ prettyExpr 1 expr

prettyDecl :: Int -> Declaration -> String
prettyDecl lvl (DValue nn [] bb) =
  ii ++ "Value" ++ name ++ ": " ++ body
  where
    ii = indentation lvl
    name = prettyName nn
    body = prettyExpr (lvl + 1) bb
prettyDecl lvl (DValue nn aa bb) =
  ii ++ "Func" ++ name ++ args ++":\n" ++ body
  where
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
        

prettyExpr :: Int -> Expression -> String
prettyExpr lvl expr = (indentation lvl) ++ (prettyExpr' expr)

prettyExpr' :: Expression -> String
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

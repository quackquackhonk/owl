{-# LANGUAGE DeriveFoldable #-}

module Syntax.AST
  ( Program (..),
    makeProgram,
    Name (..),
    Declaration (..),
    Type (..),
    Expression (..),
    Statement (..),
    BinOperator (..),
    UnOperator (..),
    mkEVar,
    mkTVar,
    mkEMul,
    mkEAdd,
    mkESub,
    mkEDiv,
    mkEEq,
    mkENeq,
    mkELt,
    mkELtEq,
    mkEGt,
    mkEGtEq,
    mkEAnd,
    mkEOr,
    mkFuncType,
    repeatFuncType,
    unTok,
    info,
    (<->),
  )
where

import Data.Maybe (fromJust)
import Data.Monoid (First (..))
import qualified Syntax.Lexer as L

-- Each constructor uses a polynomial type variable to store metadata along with the AST

data Program = Program [Declaration] (Expression)
  deriving (Show, Eq)

makeProgram :: [Declaration] -> Expression -> Program
makeProgram decls exprs = Program decls exprs

data Name = Name String
  deriving (Ord, Show, Eq)

data Declaration
  = DValue Name [Name] Expression
  | DType Name Type
  deriving (Show, Eq)

data Type
  = TVar Name
  | TUnit
  | TInt
  | TBool
  | TParen Type
  | TArrow Type Type
  deriving (Show, Eq)

data Expression
  = EUnit
  | EInt Integer
  | EBool Bool
  | EVar Name
  | EParen Expression
  | EBinaryOp BinOperator Expression Expression
  | EUnaryOp UnOperator Expression
  | ECond Expression Expression Expression
  | EFunc [Name] Expression
  | ECall Expression Expression
  | ESeq [Statement]
  deriving (Show, Eq)

data Statement
  = Expr Expression
  | Decl Declaration
  deriving (Show, Eq)

data BinOperator
  = BOAdd
  | BOSub
  | BOMul
  | BODiv
  | BOEq
  | BONeq
  | BOLt
  | BOLtEq
  | BOGt
  | BOGtEq
  | BOAnd
  | BOOr
  deriving (Show, Eq)

data UnOperator
  = UONeg
  | UONot
  deriving (Show, Eq)

-- Convenience Functions for making ASTs

mkEVar :: String -> Expression
mkEVar nn = EVar . Name $ nn

mkTVar :: String -> Type
mkTVar nn = TVar . Name $ nn

mkBinOp :: BinOperator -> Expression -> Expression -> Expression
mkBinOp op ll rr = EBinaryOp op ll rr

mkEMul :: Expression -> Expression -> Expression
mkEMul = mkBinOp BOMul

mkEAdd :: Expression -> Expression -> Expression
mkEAdd = mkBinOp BOAdd

mkESub :: Expression -> Expression -> Expression
mkESub = mkBinOp BOSub

mkEDiv :: Expression -> Expression -> Expression
mkEDiv = mkBinOp BODiv

mkEEq :: Expression -> Expression -> Expression
mkEEq = mkBinOp BOEq

mkENeq :: Expression -> Expression -> Expression
mkENeq = mkBinOp BONeq

mkELt :: Expression -> Expression -> Expression
mkELt = mkBinOp BOLt

mkELtEq :: Expression -> Expression -> Expression
mkELtEq = mkBinOp BOLtEq

mkEGt :: Expression -> Expression -> Expression
mkEGt = mkBinOp BOGt

mkEGtEq :: Expression -> Expression -> Expression
mkEGtEq = mkBinOp BOGtEq

mkEAnd :: Expression -> Expression -> Expression
mkEAnd = mkBinOp BOAnd

mkEOr :: Expression -> Expression -> Expression
mkEOr = mkBinOp BOOr


mkFuncType :: [Type] -> Type
mkFuncType [] = TVar $ Name "Unit"
mkFuncType [t] = t
mkFuncType (t : ts) = foldr TArrow t ts

repeatFuncType :: Int -> Type -> Type
repeatFuncType nn tt = mkFuncType $ take nn $ repeat tt

-- | Build a simple node by extracting its token type and range.
unTok :: L.RangedToken -> (L.Range -> L.Token -> a) -> a
unTok (L.RangedToken tok range) ctor = ctor range tok

-- | Unsafely extracts the the metainformation field of a node.
info :: Foldable f => f a -> a
info = fromJust . getFirst . foldMap pure

-- | Performs the union of two ranges by creating a new range starting at the
-- start position of the first range, and stopping at the stop position of the
-- second range.
-- Invariant: The LHS range starts before the RHS range.
(<->) :: L.Range -> L.Range -> L.Range
L.Range a1 _ <-> L.Range _ b2 = L.Range a1 b2

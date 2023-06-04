{-# LANGUAGE DeriveFoldable #-}

module Ast
  ( Program (..),
    Name(..),
    Declaration (..),
    Type (..),
    Expression (..),
    Statement(..),
    BinOperator(..),
    UnOperator(..),
    unTok,
    info,
    (<->),
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))
import Text.Pretty.Simple (pPrint)
import qualified Lexer as L

-- Each constructor uses a polynomial type variable to store metadata along with the AST

data Program a = Program [Declaration a] (Expression a)
  deriving (Foldable, Show)

data Name a = Name a ByteString
  deriving (Foldable, Show)

data Declaration a
  = DValue a (Name a) [Name a] (Expression a)
  | DType a (Name a) (Type a)
  deriving (Foldable, Show)

data Type a
  = TVar a (Name a)
  | TParen a (Type a)
  | TArrow a (Type a) (Type a)
  deriving (Foldable, Show)

data Expression a
  = EUnit a
  | EInt a Integer
  | EBool a Bool
  | EVar a (Name a)
  | EParen a (Expression a)
  | EBinaryOp a (BinOperator a) (Expression a) (Expression a)
  | EUnaryOp a (UnOperator a) (Expression a)
  | ECond a (Expression a) (Expression a) (Expression a) 
  | ECall a (Expression a) (Expression a)
  | ESeq a [Statement a]
  deriving (Foldable, Show)

data Statement a
  = Expr a (Expression a)
  | Decl a (Declaration a)
  deriving (Foldable, Show)

data BinOperator a
  = BOAdd a
  | BOSub a 
  | BOMul a
  | BODiv a
  | BOEq a
  | BONeq a
  | BOLt a
  | BOLtEq a
  | BOGt a
  | BOGtEq a
  | BOAnd a
  | BOOr a
  deriving (Foldable, Show)

data UnOperator a
  = Neg a
  | Not a
  deriving (Foldable, Show)


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

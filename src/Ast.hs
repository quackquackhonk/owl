{-# LANGUAGE DeriveFoldable #-}

module Ast
  ( Program (..),
    Name(..),
    Declaration (..),
    Expression (..),
    Type (..),
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
  = ValueDecl a (Name a) [Name a] (Expression a)
  | TypeDecl a (Name a) (Type a)
  deriving (Foldable, Show)

data Expression a
  = Unit a
  | IntLiteral a Integer
  | BoolLiteral a Bool
  | Var a (Name a)
  | Conditional a (Expression a) (Expression a) (Expression a)
  | FunCall a (Expression a) [Expression a]
  | Sequence a [Expression a]
  deriving (Foldable, Show)

data Type a
  = VarTy a (Name a)
  | FuncTy a [Type a] (Type a)
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

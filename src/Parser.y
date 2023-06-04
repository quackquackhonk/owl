{
module Parser
  ( parseOwl,
    printAst
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Text.Pretty.Simple (pPrint)

import Ast
import qualified Lexer as L

}

%name parseOwl program
%tokentype { L.RangedToken }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.RangedToken L.EOF _ }
%expect 0

%token
  -- Identifiers
  ident      { L.RangedToken (L.Identifier _) _ }
  -- Constants
  integer    { L.RangedToken (L.Integer _) _ }
  boolean    { L.RangedToken (L.Boolean _) _ }
  -- Keywords
  let        { L.RangedToken L.Let _ }
  be         { L.RangedToken L.Be _ }
  if         { L.RangedToken L.If _ }
  then       { L.RangedToken L.Then _ }
  else       { L.RangedToken L.Else _ }
  -- Arithmetic operators
  '+'        { L.RangedToken L.Plus _ }
  '-'        { L.RangedToken L.Minus _ }
  '*'        { L.RangedToken L.Times _ }
  '/'        { L.RangedToken L.Divide _ }
  -- Comparison operators
  '='        { L.RangedToken L.Eq _ }
  '!='       { L.RangedToken L.Neq _ }
  '<'        { L.RangedToken L.Lt _ }
  '<='       { L.RangedToken L.Lteq _ }
  '>'        { L.RangedToken L.Gt _ }
  '>='       { L.RangedToken L.Gteq _ }
  -- Logical operators
  '&'        { L.RangedToken L.And _ }
  '|'        { L.RangedToken L.Or _ }
  '!'        { L.RangedToken L.Not _ }
  -- Parenthesis
  '('        { L.RangedToken L.LParen _ }
  ')'        { L.RangedToken L.RParen _ }
  -- Lists
  '{'        { L.RangedToken L.LBrace _ }
  '}'        { L.RangedToken L.RBrace _ }
  ';'        { L.RangedToken L.SemiColon _ }
  -- Types
  typ        { L.RangedToken L.Type _ }
  is         { L.RangedToken L.Is _ }
  '->'       { L.RangedToken L.Arrow _ }

-- type arrows are right associative
%right else
%right '->'

%left '|'
%left '&'
%nonassoc '=' '!=' '<' '>' '<=' '>=' '!'
%left '+' '-'
%left '*' '/'

%%

many_rev(p)
:               { [] }
| many_rev(p) p { $2 : $1 }

many(p)
: many_rev(p) { reverse $1 }

sepBy_rev(p, sep)
:                         { [] }
| sepBy_rev(p, sep) sep p { $3 : $1 }

sepBy(p, sep)
: sepBy_rev(p, sep) { reverse $1 }

optional(p)
:          { Nothing }
| p        { Some $1 }

program :: { Program L.Range }
: decls expr { Program $1 $2 }

name :: { Name L.Range }
: ident { unTok $1 (\rr (L.Identifier name) -> Name rr name)}

decls :: { [Declaration L.Range] }
: many(decl) { $1 }

decl :: { Declaration L.Range }
: let name many(name) be expr ';' { DValue (L.rtRange $1 <-> L.rtRange $6) $2 $3 $5 }
| typ name is type                { DType (L.rtRange $1 <-> info $4) $2 $4 }

expr :: { Expression L.Range }
: exprapp    { $1 }
| exprcond   { $1 }
| exprseq    { $1 }
| '-' expr   { EUnaryOp (L.rtRange $1 <-> info $2) (Neg $ L.rtRange $1) $2 }
| '!' expr   { EUnaryOp (L.rtRange $1 <-> info $2) (Not $ L.rtRange $1) $2 }
-- arithmetic
| expr '+' expr { EBinaryOp (info $1 <-> info $3) (BOAdd (L.rtRange $2)) $1 $3 }
| expr '-' expr { EBinaryOp (info $1 <-> info $3) (BOSub (L.rtRange $2)) $1 $3 }
| expr '*' expr { EBinaryOp (info $1 <-> info $3) (BOMul (L.rtRange $2)) $1 $3 }
| expr '/' expr { EBinaryOp (info $1 <-> info $3) (BODiv (L.rtRange $2)) $1 $3 }
-- comparison
| expr '=' expr  { EBinaryOp (info $1 <-> info $3) (BOEq  (L.rtRange $2)) $1 $3 }
| expr '!=' expr { EBinaryOp (info $1 <-> info $3) (BONeq (L.rtRange $2)) $1 $3 }
| expr '<' expr  { EBinaryOp (info $1 <-> info $3) (BOLt  (L.rtRange $2)) $1 $3 }
| expr '>' expr  { EBinaryOp (info $1 <-> info $3) (BOGt  (L.rtRange $2)) $1 $3 }
| expr '<=' expr { EBinaryOp (info $1 <-> info $3) (BOLtEq (L.rtRange $2)) $1 $3 }
| expr '>=' expr { EBinaryOp (info $1 <-> info $3) (BOGtEq (L.rtRange $2)) $1 $3 }
-- logic
| expr '&' expr { EBinaryOp (info $1 <-> info $3) (BOAnd (L.rtRange $2)) $1 $3 }
| expr '|' expr { EBinaryOp (info $1 <-> info $3) (BOOr  (L.rtRange $2)) $1 $3 }

exprapp :: { Expression L.Range }
: exprapp atom { ECall (info $1 <-> info $2) $1 $2 }
| atom      { $1 }

exprcond :: {Expression L.Range}
: if expr then expr else expr { ECond (L.rtRange $1 <-> info $6) $2 $4 $6 }

exprseq :: { Expression L.Range }
: '{' sepBy(stmt, ';') '}' { ESeq (L.rtRange $1 <-> L.rtRange $3) $2 }

stmt :: { Statement L.Range }
: decl  { Decl (info $1) $1 }
| expr  { Expr (info $1) $1 }

atom :: { Expression L.Range }
: '(' expr ')'                     { EParen (L.rtRange $1 <-> L.rtRange $3) $2 }
| '('')'                           { EUnit $ L.rtRange $1 <-> L.rtRange $2 }
| name                             { EVar (info $1) $1 }
| boolean                          { unTok $1 (\rr (L.Boolean bb) -> EBool rr bb)}
| integer                          { unTok $1 (\rr (L.Integer int) -> EInt rr int)}

type ::   { Type L.Range }
: name           { TVar (info $1) $1 }
| '(' type ')'   { TParen (L.rtRange $1 <-> L.rtRange $3) $2 }
| type '->' type { TArrow (info $1 <-> info $3) $1 $3 }

{
parseError :: L.RangedToken -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

printAst :: ByteString -> IO ()
printAst str = case L.runAlex str parseOwl of
    Left e -> print $ show e
    Right ast -> pPrint ast
    

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)
}

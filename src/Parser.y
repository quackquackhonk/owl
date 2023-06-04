{
module Parser
  ( parseOwl,
    astFromStr
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)

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
  '{'        { L.RangedToken L.LBrace _ }
  '}'        { L.RangedToken L.RBrace _ }
  ';'        { L.RangedToken L.SemiColon _ }
  '$'        { L.RangedToken L.Dollar _ }
  -- Anon Functions
  '@' { L.RangedToken L.At _ }
  '=>' { L.RangedToken L.ThickArrow _ }
  -- Types
  typ        { L.RangedToken L.Type _ }
  is         { L.RangedToken L.Is _ }
  '->'       { L.RangedToken L.Arrow _ }
  eof        { L.RangedToken L.EOF _ }   

-- type arrows are right associative
%right '=>'
%right else
%right '->'

%right '$'
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
: p                       { [$1] }
| sepBy_rev(p, sep) sep p { $3 : $1 }

sepBy(p, sep)
: sepBy_rev(p, sep) { reverse $1 }

program :: { Program }
: decls expr { Program $1 $2 }

name :: { Name }
: ident { unTok $1 (\rr (L.Identifier name) -> Name name)}

decls :: { [Declaration] }
: many(semi_decl) { $1 }

semi_decl :: { Declaration }
: decl ';'        { $1 }

decl :: { Declaration }
: let name many(name) be expr { DValue $2 $3 $5 }
| typ name is type   { DType  $2 $4 }

expr :: { Expression }
: exprapp    { $1 }
| exprcond   { $1 }
| '-' expr   { EUnaryOp UONeg $2 }
| '!' expr   { EUnaryOp UONot $2 }
-- anon functions
| '@' many(name) '=>' expr { EFunc $2 $4 }
-- apply
| expr '$' expr { ECall $1 $3 }
-- arithmetic
| expr '+' expr { EBinaryOp  (BOAdd ) $1 $3 }
| expr '-' expr { EBinaryOp  (BOSub ) $1 $3 }
| expr '*' expr { EBinaryOp  (BOMul ) $1 $3 }
| expr '/' expr { EBinaryOp  (BODiv ) $1 $3 }
-- comparison
| expr '=' expr  { EBinaryOp  (BOEq  ) $1 $3 }
| expr '!=' expr { EBinaryOp  (BONeq ) $1 $3 }
| expr '<' expr  { EBinaryOp  (BOLt  ) $1 $3 }
| expr '>' expr  { EBinaryOp  (BOGt  ) $1 $3 }
| expr '<=' expr { EBinaryOp  (BOLtEq ) $1 $3 }
| expr '>=' expr { EBinaryOp  (BOGtEq ) $1 $3 }
-- logic
| expr '&' expr { EBinaryOp  (BOAnd ) $1 $3 }
| expr '|' expr { EBinaryOp  (BOOr  ) $1 $3 }

exprapp :: { Expression }
: exprapp atom { ECall $1 $2 }
| atom      { $1 }

exprcond :: { Expression }
: if expr then expr else expr { ECond  $2 $4 $6 }

atom :: { Expression }
: '('')'                           { EUnit }
| '{' sepBy(stmt, ';') '}'         { ESeq $2 }
| '(' expr ')'                     { EParen  $2 }
| name                             { EVar $1 }
| boolean                          { unTok $1 (\rr (L.Boolean bb) -> EBool bb)}
| integer                          { unTok $1 (\rr (L.Integer int) -> EInt int)}

stmt :: { Statement }
: expr  { Expr  $1 }
| decl  { Decl  $1 }

type ::   { Type }
: name           { parseTVar $1 }
| '(' type ')'   { TParen $2 }
| type '->' type { TArrow $1 $3 }

{
parseError :: L.RangedToken -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

parseTVar tt =
    case tt of
      Name "Unit" -> TUnit
      Name "Int"  -> TInt
      Name "Bool" -> TBool
      Name xx -> TVar (Name xx)

-- astFromStr :: ByteString -> Either (L.ParseError) (Program L.Range) 
astFromStr str = case L.runAlex str parseOwl of
    Left e -> error $ show e
    Right ast -> ast


lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)
}

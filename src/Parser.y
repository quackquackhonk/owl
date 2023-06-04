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

%token
  -- Identifiers
  ident      { L.RangedToken (L.Identifier _) _ }
  -- Constants
  integer    { L.RangedToken (L.Integer _) _ }
  boolean    { L.RangedToken (L.Boolean _) _ }
  -- Keywords
  let        { L.RangedToken L.Let _ }
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
  -- Parenthesis
  '('        { L.RangedToken L.LParen _ }
  ')'        { L.RangedToken L.RParen _ }
  -- Lists
  '{'        { L.RangedToken L.LBrace _ }
  '}'        { L.RangedToken L.RBrace _ }
  ','        { L.RangedToken L.Comma _ }
  ';'        { L.RangedToken L.SemiColon _ }
  -- Types
  typ        { L.RangedToken L.Type _ }
  is         { L.RangedToken L.Is _ }
  '->'       { L.RangedToken L.Arrow _ }

%%

program :: { Program L.Range }
: decls expr { Program $1 $2 }

name :: { Name L.Range }
: ident { unTok $1 (\rr (L.Identifier name) -> Name rr name)}

decls :: { [Declaration L.Range] }
: many(decl) { $1 }

decl :: { Declaration L.Range }
: let name many(name) '=' expr { ValueDecl (L.rtRange $1 <-> info $5) $2 $3 $5 }
| typ name is type             { TypeDecl (L.rtRange $1 <-> info $4) $2 $4 }

expr :: { Expression L.Range }
: '('')'  { Unit $ L.rtRange $1 <-> L.rtRange $2 }
| name    { Var (info $1) $1 }
| integer { unTok $1 (\rr (L.Integer int) -> IntLiteral rr int)}

type ::   { Type L.Range }
: name    { VarTy (info $1) $1 }

many_rev(p)
:               { [] }
| many_rev(p) p { $2 : $1 }

many(p)
: many_rev(p) { reverse $1 }

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

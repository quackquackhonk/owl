{
module Lexer
  ( -- * Invoking Alex
    Alex
  , AlexPosn (..)
  , alexGetInput
  , alexError
  , runAlex
  , alexMonadScan

  , Range (..)
  , RangedToken (..)
  , Token (..)
  , scanMany
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Monad (when)
}

%wrapper "monadUserState-bytestring"

$digit = [0-9]
$alpha = [a-zA-Z]

@id = ($alpha | \_) ($alpha | $digit | \_ | \' | \?)*

tokens :-

<0> $white+ ;

<0> "#*" { nestComment `andBegin` comment }
<0> "*#" { \_ _ -> alexError "Error: unexpected closing comment" }
<comment> "#*" { nestComment }
<comment> "*#" { unnestComment }
<comment> . ;
<comment> \n ;

<0> "#" .*;

<0> let     { tok Let }
<0> be      { tok Be }
<0> if      { tok If }
<0> then    { tok Then }
<0> else    { tok Else }

<0> "+"     { tok Plus }
<0> "-"     { tok Minus }
<0> "*"     { tok Times }
<0> "/"     { tok Divide }

<0> "="     { tok Eq }
<0> "!="      { tok Neq }
<0> "<"     { tok Lt }
<0> ">"     { tok Gt }
<0> "<="    { tok Lteq }
<0> ">="    { tok Gteq }

<0> "&"     { tok And }
<0> "|"      { tok Or }
<0> "!"     { tok Not }

<0> "("     { tok LParen }
<0> ")"     { tok RParen }

<0> "{"     { tok LBrace }
<0> "}"     { tok RBrace }
<0> ";"     { tok SemiColon }

<0> typ     { tok Type }
<0> is      { tok Is }
<0> "->"    { tok Arrow }

<0> true    { tok $ Boolean True }
<0> false   { tok $ Boolean False }

<0> @id     { tokId }

<0> $digit+ { tokInt }

{
data AlexUserState = AlexUserState
  { nestLevel :: Int
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { nestLevel = 0 }

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq, Show)

data RangedToken = RangedToken
  { rtToken :: Token
  , rtRange :: Range
  } deriving (Eq, Show)

data Token
  -- Identifiers
  = Identifier ByteString
  -- Constants
  | Integer Integer
  | Boolean Bool
  -- Keywords
  | Let
  | Be
  | If
  | Then
  | Else
  -- Arithmetic operators
  | Plus
  | Minus
  | Times
  | Divide
  -- Comparison operators
  | Eq
  | Neq
  | Lt
  | Lteq
  | Gt
  | Gteq
  -- Logical operators
  | And
  | Or
  | Not
  -- Parenthesis
  | LParen
  | RParen
  -- Sequences
  | LBrace
  | RBrace
  | SemiColon
  -- Types
  | Type
  | Is
  | Arrow
  -- EOF
  | EOF
  deriving (Eq, Show)

mkRange :: AlexInput -> Int64 -> Range
mkRange (start, _, str, _) len = Range{start = start, stop = stop}
  where
    stop = BS.foldl' alexMove start $ BS.take len str

tokId :: AlexAction RangedToken
tokId inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Identifier $ BS.take len str
    , rtRange = mkRange inp len
    }

tok :: Token -> AlexAction RangedToken
tok tt inp len =
  pure RangedToken
    { rtToken = tt
    , rtRange = mkRange inp len
    }

tokInt :: AlexAction RangedToken
tokInt inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Integer $ read $ BS.unpack $ BS.take len str
    , rtRange = mkRange inp len
    }

nestComment, unnestComment :: AlexAction RangedToken
nestComment input len = do
  modify $ \s -> s{nestLevel = nestLevel s + 1}
  skip input len
unnestComment input len = do
  state <- get
  let level = nestLevel state - 1
  put state{nestLevel = level}
  when (level == 0) $
    alexSetStartCode 0
  skip input len

alexEOF :: Alex RangedToken
alexEOF = do
  startCode <- alexGetStartCode
  when (startCode == comment) $
    alexError "Error: unclosed comment"
  (pos, _, _, _) <- alexGetInput
  pure $ RangedToken EOF (Range pos pos)

scanMany :: ByteString -> Either String [RangedToken]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if rtToken output == EOF
        then pure [output]
        else (output :) <$> go
}

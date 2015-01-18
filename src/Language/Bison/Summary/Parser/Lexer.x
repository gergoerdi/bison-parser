-- -*- haskell -*-
{
module Language.Bison.Summary.Parser.Lexer
       ( scanner
       , SrcLoc(..)
       , Loc(..)
       , Token(..)
       ) where

import Data.Function (fix)
}

%wrapper "monad"

$digit      = [0-9]
$start      = [a-zA-Z_]
$cont       = [a-zA-Z_0-9]
@ident      = ($start)($cont*)
@space      = $white # \n

tokens :-

<0>     ^Grammar\n               { begin rules }
<0>     .*\n                     { skip }
<rules> ^@space*$digit+$white    { begin line }
<rules> ^@space*\n               { accept_ EOR }
<rules> ^[a-zA-Z]                { begin outro }
<outro> .*\n                     { skip }

<line>  @space+                  { skip }
<line>  \n                       { begin rules }
<line>  @ident                   { accept Id }
<line>  \$@ident                 { accept (Meta . tail) }
<line>  \%@ident                 { accept (Directive . tail) }
<line>  :                        { accept_ Colon }
<line>  \|                       { accept_ Bar }
<line>  '[^\\]'                  { accept (CharLit . (!!1)) }

{
data SrcLoc = SrcLoc !Int !Int
            deriving Show

data Loc a = L{ getLoc :: SrcLoc, unLoc :: a }
           deriving Show

data Token = Id String
           | Meta String
           | Directive String
           | CharLit Char
           | Colon
           | Bar
           | EOR
           deriving Show

accept :: (String -> a)
       -> (AlexPosn, Char, [Byte], [Char])
       -> Int -> Alex (Maybe (Loc a))
accept f (pos, _, _, str) len = return . Just . L loc . f $ take (fromIntegral len) str
  where
    AlexPn _ line col = pos
    loc = SrcLoc line col

accept_ :: a
        -> (AlexPosn, Char, [Byte], [Char])
        -> Int
        -> Alex (Maybe (Loc a))
accept_ = accept . const

alexEOF = return Nothing

scanner str = runAlex str $ fix $ \loop -> do
    tok <- alexMonadScan
    case tok of
        Nothing -> return []
        Just tok -> do
            toks <- loop
            return (tok:toks)
}

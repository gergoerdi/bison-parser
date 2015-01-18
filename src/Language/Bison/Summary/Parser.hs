{-# LANGUAGE LambdaCase #-}
module Language.Bison.Summary.Parser
       ( parseRules
       ) where

import Language.Bison.Summary.Parser.Lexer
import Language.Bison.Summary.Syntax

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Pos
import Text.Parsec.Error
import Control.Applicative hiding ((<|>), optional)
import Control.Monad
import qualified Data.Map as Map

type Parser = Parsec [Loc Token] ()

parseRules :: SourceName -> String -> Either ParseError [Rule]
parseRules src stream = postproc <$> parseAs rules src stream

postproc :: [Rule] -> [Rule]
postproc rules = [ Rule name rhss'
                 | (name, rhss) <- Map.toList rulesMap
                 , let rhss' = map (map resolve) rhss
                 ]
  where
    rulesMap = Map.unionsWith (++)
               [ Map.singleton name rhss
               | Rule name rhss <- rules
               ]

    isTerminal s = RuleName s `Map.notMember` rulesMap

    resolve (Nonterminal (RuleName name)) | isTerminal name = Terminal name
    resolve element = element

parseAs :: Parser a -> SourceName -> String -> Either ParseError a
parseAs p src stream = case scanner stream of
    Left err -> Left $ newErrorMessage
                (Message $ unwords ["lexer failed:", err])
                (newPos src 0 0)
    Right tokens -> runParser p () src tokens

rules :: Parser [Rule]
rules = skipMany eor >> rule `endBy` many1 eor

rule :: Parser Rule
rule = Rule <$> name <* colon <*> (alt `sepBy` bar) <?> "grammar rule"

alt :: Parser [Element]
alt = (directive "empty" *> pure []) <|>
      (many1 element) <?>
      "grammar alternative"

element :: Parser Element
element = (Nonterminal <$> name) <|>
          (Lit <$> charLit) <?>
          "grammar element"

name :: Parser Name
name = tok $ \case
    Id s -> return $ RuleName s
    Meta s -> return $ MetaName s
    _ -> Nothing

charLit :: Parser Char
charLit = tok $ \case
    CharLit c -> return c
    _ -> Nothing

colon :: Parser ()
colon = tok $ \case
    Colon -> return ()
    _ -> Nothing

bar :: Parser ()
bar = tok $ \case
    Bar -> return ()
    _ -> Nothing

eor :: Parser ()
eor = tok $ \case
    EOR -> return ()
    _ -> Nothing

directive :: String -> Parser ()
directive s = tok $ \case
    Directive s' -> guard (s == s')
    _ -> Nothing

tok :: (Show t) => (t -> Maybe a) -> Parsec [Loc t] s a
tok f = do
    src <- sourceName <$> getPosition
    let fromLoc (SrcLoc line col) = newPos src line col
    token (show . unLoc) (fromLoc . getLoc) (f . unLoc)

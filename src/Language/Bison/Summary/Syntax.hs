module Language.Bison.Summary.Syntax
       ( Name(..)
       , Rule(..)
       , Element(..)
       ) where

data Name = RuleName String
          | MetaName String
          deriving (Show, Eq, Ord)

data Rule = Rule Name [[Element]]
          deriving Show

data Element = Nonterminal Name
             | Terminal String
             | Lit Char
             deriving Show

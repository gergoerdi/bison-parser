module Main where

import System.Environment
import Language.Bison.Summary.Syntax
import Language.Bison.Summary.Parser
import Control.Applicative

main = do
  args <- getArgs
  (src, stream) <- case args of
    [filename] -> (filename,) <$> readFile filename
    [] -> ("",) <$> getContents
  case parseRules src stream of
    Left err -> print err
    Right rules -> mapM_ print rules

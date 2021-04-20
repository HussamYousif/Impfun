module Main where

import Syntax
import Parser
import Pretty
import Eval
import Data.Either
import System.Environment (getArgs)




-- Functionality implemented:
-- Return statements.
-- Try catch throw statements.
-- Lists with the following operations: concat (++) operator, extract element (!!) operator.
-- For loops.

main = do
  args <- getArgs
  let fname = if null args then error "no filename" else head args
  input <- readFile fname
  case parse program fname input of
    Right v -> do
      --putStrLn $ pPrint v
      exec v >> return ()
    Left e -> print e

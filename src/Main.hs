module Main where

import System.Environment

import Lexer
import Parser
import Types
import SemanticAnalysis
import Typecheck

main :: IO ()
main = do
  args <- getArgs
  str <- readFile (head args)
  let ast = parse str
  let program = collectPieces ast
  print $ semanticChecks program

-- Parse: putStrLn . show $ parse str
-- Lex:  putStrLn . show $ alexTokens str

-- Lex: alexTokens :: String -> [Token]
-- Lex: alexTokens = evalP test

-- Lex: test :: P [Token]
-- Lex: test = do
-- Lex:   token <- readToken
-- Lex:   if (unTok token) == TEOF
-- Lex:   then return [token]
-- Lex:   else test >>= return . (token :)

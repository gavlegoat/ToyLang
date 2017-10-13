module Main where

import System.Environment

import Lexer

main :: IO ()
main = do
  args <- getArgs
  str <- readFile (head args)
  putStrLn . show $ alexTokens str

alexTokens :: String -> [Token]
alexTokens = evalP test

test :: P [Token]
test = do
  token <- readToken
  if (unTok token) == TEOF
  then return [token]
  else test >>= return . (token :)

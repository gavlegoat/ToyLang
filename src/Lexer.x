{
module Lexer where

import Control.Monad.State
import Control.Monad
import Data.Word
import Codec.Binary.UTF8.String (encode)
}

@id = [a-z_][A-Za-z0-9_]*
@type = [A-Z][A-Za-z0-9_]*
@int = "-"?[0-9]+

tokens :-
<0>        $white+   ;
<0>        "--".*$   ;
<0>        ";"       { \_ _ -> mkPlainToken TSemi }
<0,comSC>  "{*"      { beginComment }
<comSC>    "*}"      { endComment }
<comSC>    [.\n]     ;
<0>        "("       { \_ _ -> mkPlainToken TLParen }
<0>        ")"       { \_ _ -> mkPlainToken TRParen }
<0>        "is"      { \_ _ -> mkPlainToken TIs }
<0>        "and"     { \_ _ -> mkPlainToken TFunAnd }
<0>        "|"       { \_ _ -> mkPlainToken TBar }
<0>        @type     { \_ s -> mkPlainToken $ TType s }
<0>        "=>"      { \_ _ -> mkPlainToken TDoubleArrow }
<0>        "="       { \_ _ -> mkPlainToken TEquals }
<0>        "data"    { \_ _ -> mkPlainToken TData }
<0>        @int      { \_ s -> mkPlainToken $ TInt (read s) }
<0>        \"        { beginString }
<strSC>    \\[nt\"]  { escapeString }
<strSC>    \"        { endString }
<strSC>    .         { appendString }
<0>        "true"    { \_ _ -> mkPlainToken $ TBool True }
<0>        "false"   { \_ _ -> mkPlainToken $ TBool False }
<0>        "+"       { \_ _ -> mkPlainToken TAdd }
<0>        "-"       { \_ _ -> mkPlainToken TSub }
<0>        "*"       { \_ _ -> mkPlainToken TMul }
<0>        "/"       { \_ _ -> mkPlainToken TDiv }
<0>        "~"       { \_ _ -> mkPlainToken TNeg }
<0>        "<"       { \_ _ -> mkPlainToken TLt }
<0>        "<="      { \_ _ -> mkPlainToken TLe }
<0>        "=="      { \_ _ -> mkPlainToken TEq }
<0>        "&&"      { \_ _ -> mkPlainToken TAnd }
<0>        "||"      { \_ _ -> mkPlainToken TOr }
<0>        "not"     { \_ _ -> mkPlainToken TNot }
<0>        ","       { \_ _ -> mkPlainToken TComma }
<0>        ":"       { \_ _ -> mkPlainToken TColon }
<0>        "["       { \_ _ -> mkPlainToken TLSquare }
<0>        "]"       { \_ _ -> mkPlainToken TRSquare }
<0>        "let"     { \_ _ -> mkPlainToken TLet }
<0>        "in"      { \_ _ -> mkPlainToken TIn }
<0>        "case"    { \_ _ -> mkPlainToken TCase }
<0>        "of"      { \_ _ -> mkPlainToken TOf }
<0>        "if"      { \_ _ -> mkPlainToken TIf }
<0>        "then"    { \_ _ -> mkPlainToken TThen }
<0>        "else"    { \_ _ -> mkPlainToken TElse }
<0>        "->"      { \_ _ -> mkPlainToken TSingleArrow }
<0>        @id       { \_ s -> mkPlainToken $ TId s }

{

data TokenType = TId String
               | TSemi
               | TLParen
               | TRParen
               | TIs
               | TFunAnd  -- and
               | TBar  -- |
               | TType String
               | TDoubleArrow  -- =>
               | TEquals  -- =
               | TData
               | TInt Int
               | TString String
               | TBool Bool
               | TAdd
               | TSub
               | TMul
               | TDiv
               | TNeg
               | TLt
               | TLe
               | TEq  -- ==
               | TAnd  -- &&
               | TOr
               | TNot
               | TComma
               | TColon
               | TLSquare
               | TRSquare
               | TLet
               | TIn
               | TCase
               | TOf
               | TIf
               | TThen
               | TElse
               | TSingleArrow  -- ->
               | TEOF
  deriving (Eq, Show)

data Token = Token { unTok :: TokenType, lineno :: Int } deriving (Show)

mkPlainToken :: TokenType -> P (Maybe Token)
mkPlainToken t = getLineNo >>= return . Just . Token t

-- Most of the rest of this file is taken from
-- https://github.com/jmoy/alexhappy/blob/master/startcode/


data AlexInput = AlexInput { aiprev :: Char
                           , aibytes :: [Word8]
                           , airest :: String
                           , ailineno :: Int }
                           deriving (Show)

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte ai = case (aibytes ai) of
  (b : bs) -> Just (b, ai { aibytes=bs })
  [] -> case (airest ai) of
    [] -> Nothing
    (c : cs) -> let n = (ailineno ai)
                    n' = if c == '\n' then n+1 else n
                    (b : bs) = encode [c] in
                 Just (b, AlexInput { aiprev = c
                                    , aibytes = bs
                                    , airest = cs
                                    , ailineno = n' })

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = aiprev

data ParseState = ParseState { input :: AlexInput
                             , lexSC :: Int
                             , commentDepth :: Int
                             , strBuf :: String }
                             deriving (Show)

initialState :: String -> ParseState
initialState s = ParseState { input = AlexInput { aiprev = '\n'
                                                , aibytes = []
                                                , airest = s
                                                , ailineno = 1 }
                            , lexSC = 0
                            , commentDepth = 0
                            , strBuf = "" }

type P a = State ParseState a

getLineNo :: P Int
getLineNo = do
  s <- get
  return . ailineno . input $ s

evalP :: P a -> String -> a
evalP m s = evalState m (initialState s)

type LexAction = Int -> String -> P (Maybe Token)

beginString :: LexAction
beginString _ _ = do
  s <- get
  put s { lexSC = strSC }
  return Nothing

appendString :: LexAction
appendString _ (c : _) = do
  s <- get
  put s { strBuf = c : strBuf s }
  return Nothing

escapeString :: LexAction
escapeString _ (_ : c : _) = do
  let unesc =
        case c of
          'n' -> '\n'
          't' -> '\t'
          '"' -> '"'
  s <- get
  put s { strBuf = unesc : strBuf s }
  return Nothing

endString :: LexAction
endString _ _ = do
  s <- get
  let buf = strBuf s
  put s { lexSC = 0, strBuf = "" }
  mkPlainToken (TString $ reverse buf)

beginComment :: LexAction
beginComment _ _ = do
  s <- get
  put s { lexSC = comSC, commentDepth = commentDepth s + 1 }
  return Nothing

endComment :: LexAction
endComment _ _ = do
  s <- get
  let cd = commentDepth s
  let sc' = if cd == 1 then 0 else comSC
  put s { lexSC = sc', commentDepth = cd - 1 }
  return Nothing

readToken :: P Token
readToken = do
  s <- get
  case alexScan (input s) (lexSC s) of
    AlexEOF -> if (lexSC s) == comSC then error "Lexer: End of file in comment"
               else if (lexSC s) == strSC then error "Lexer: End of file in string"
               else return (Token TEOF 0)
    AlexError inp' -> error $ [aiprev inp'] -- "Lexical error on line " ++ (show $ ailineno inp')
    AlexSkip inp' _ -> do
      put s { input = inp' }
      readToken
    AlexToken inp' n act -> do
      let (AlexInput { airest = buf }) = input s
      put s { input = inp' }
      res <- act n (take n buf)
      case res of
        Nothing -> readToken
        Just t -> return t

lexer :: (Token -> P a) -> P a
lexer cont = do
  tok <- readToken
  cont tok

}

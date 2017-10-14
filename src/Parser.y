{
module Parser (parse) where

import Control.Monad.State

import Lexer
import Types
}

%name parserAct
%tokentype {Token}
%error {parseError}
%monad {P}
%lexer {lexer} {Token TEOF _}

%token

'('      { Token TLParen _ }
')'      { Token TRParen _ }
'is'     { Token TIs _ }
'and'    { Token TAnd _ }
'->'     { Token TSingleArrow _ }
'id'     { Token (TId _) _ }
'='      { Token TEquals _ }
'|'      { Token TBar _ }
'=>'     { Token TDoubleArrow _ }
'type'   { Token (TType _) _ }
'data'   { Token TData _ }
'int'    { Token (TInt _) _ }
'string' { Token (TString _) _ }
'true'   { Token (TBool True) _ }
'false'  { Token (TBool False) _ }
'+'      { Token TAdd _ }
'-'      { Token TSub _ }
'*'      { Token TMul _ }
'/'      { Token TDiv _ }
'~'      { Token TNeg _ }
'<'      { Token TLt _ }
'=='     { Token TEq _ }
'<='     { Token TLe _ }
'&&'     { Token TAnd _ }
'||'     { Token TOr _ }
'not'    { Token TNot _ }
','      { Token TComma _ }
':'      { Token TColon _ }
'['      { Token TLSquare _ }
']'      { Token TRSquare _ }
'let'    { Token TLet _ }
'in'     { Token TIn _ }
'case'   { Token TCase _ }
'of'     { Token TOf _ }
'if'     { Token TIf _ }
'then'   { Token TThen _ }
'else'   { Token TElse _ }

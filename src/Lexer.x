{
module Main where
}

%wrapper "monad"

$int = [\+-]?[0-9]+
$id = [a-z_][A-Za-z0-9_]*
$type = [A-Z][A-Za-z0-9_]*

-- grammar stuff goes here

%tokens :-
<0>     $white+   ;
<0>     COMMENTS
<0>     $id       (\_ s -> TId s)
<0>     "("       (\_ _ -> TLParen)
<0>     ")"       (\_ _ -> TRParen)
<0>     "is"      (\_ _ -> TIs)
<0>     "and"     (\_ _ -> TFunAnd)
<0>     $type     (\_ s -> TType s)
<0>     "=>"      (\_ _ -> TDoubleArrow)
<0>     "="       (\_ _ -> TEquals)
<0>     "data"    (\_ _ -> TData)
<0>     $int      (\_ s -> TInt (read s))
<0>     STRINGS
<0>     "true"    (\_ _ -> TBool True)
<0>     "false"   (\_ _ -> TBool False)
<0>     "+"       (\_ _ -> TAdd)
<0>     "-"       (\_ _ -> TSub)
<0>     "*"       (\_ _ -> TMul)
<0>     "/"       (\_ _ -> TDiv)
<0>     "~"       (\_ _ -> TNeg)
<0>     "<"       (\_ _ -> TLt)
<0>     "<="      (\_ _ -> TLe)
<0>     "=="      (\_ _ -> TEq)
<0>     "&&"      (\_ _ -> TAnd)
<0>     "||"      (\_ _ -> TOr)
<0>     "not"     (\_ _ -> TNot)
<0>     ":"       (\_ _ -> TColon)
<0>     "["       (\_ _ -> TLSquare)
<0>     "]"       (\_ _ -> TRSquare)
<0>     "let"     (\_ _ -> TLet)
<0>     "in"      (\_ _ -> TIn)
<0>     "case"    (\_ _ -> TCase)
<0>     "of"      (\_ _ -> TOf)
<0>     "if"      (\_ _ -> TIf)
<0>     "then"    (\_ _ -> TThen)
<0>     "else"    (\_ _ -> TElse)
<0>     "->"      (\_ _ -> TSingleArrow)

{

data Token = TId String
           | TLParen
           | TRParen
           | TIs
           | TFunAnd  -- and
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

}

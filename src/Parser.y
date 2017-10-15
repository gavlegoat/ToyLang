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
is       { Token TIs _ }
and      { Token TFunAnd _ }
'->'     { Token TSingleArrow _ }
id       { Token (TId _) _ }
'='      { Token TEquals _ }
'|'      { Token TBar _ }
'=>'     { Token TDoubleArrow _ }
type     { Token (TType _) _ }
data     { Token TData _ }
int      { Token (TInt _) _ }
string   { Token (TString _) _ }
true     { Token (TBool True) _ }
false    { Token (TBool False) _ }
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
not      { Token TNot _ }
','      { Token TComma _ }
':'      { Token TColon _ }
'['      { Token TLSquare _ }
']'      { Token TRSquare _ }
let      { Token TLet _ }
in       { Token TIn _ }
case     { Token TCase _ }
of       { Token TOf _ }
if       { Token TIf _ }
then     { Token TThen _ }
else     { Token TElse _ }

%left '->'
%left APP
%left '&&'
%left '||'
%right not
%nonassoc '<' '==' '<='
%left '+' '-'
%left '*' '/'
%right '~'

%%

Program :: { PosAST }
         : DeclList   { AST $1 }

DeclList :: { [PosDeclaration] }
          : Declaration            { [$1] }
          | DeclList Declaration   { $2 : $1 }

Declaration :: { PosDeclaration }
             : Function   { FunDecl $1 }
             | Datatype   { TypeDecl $1 }

Function :: { PosFunction }
          : id '(' FunType ')' is FunDefList   { mkFunction $1 $3 $6 }

FunType :: { Type }
         : type                   { case unTok $1 of
                                      TType s -> BaseType s
                                      _ -> error "Parse error: bad type"}
         | FunType '=>' FunType   { FunctionType $1 $3 }

FunDefList :: { [([PExpr], PosExpr)] }
            : FunDef                  { [$1] }
            | FunDefList and FunDef   { $1 ++ [$3] }

FunDef :: { ([PExpr], PosExpr) }
        : id PExprList '=' Expr   { ($2, $4) }

PExprList :: { [PExpr] }
           : PExpr             { [$1] }
           | PExprList PExpr   { $1 ++ [$2] }

PExpr :: { PExpr }
       : id                       { case unTok $1 of
                                      TId s -> PId s
                                      _ -> error "Parse error: bad ID" }
       | int                      { case unTok $1 of
                                      TInt i -> PInt i
                                      _ -> error "Parse error: bad int" }
       | string                   { case unTok $1 of
                                      TString s -> PString s
                                      _ -> error "Parse error: bad string" }
       | true                     { case unTok $1 of
                                      TBool True -> PBool True
                                      _ -> error "Parse error: bad true" }
       | false                    { case unTok $1 of
                                      TBool False -> PBool False
                                      _ -> error "Parse error: bad false" }
       | '(' ')'                  { PUnit }
       | '(' PExprCommaList ')'   { PTuple $2 }
       | '[' ']'                  { PEmptyList }
       | PExpr ':' PExpr          { PCons $1 $3 }
       | type PExprList           { PConstructor $2 }

PExprCommaList :: { [PExpr] }
                : PExpr                      { [$1] }
                | PExprCommaList ',' PExpr   { $1 ++ [$3] }

Expr :: { PosExpr }
      : id       { case unTok $1 of
                     TId s -> AnnFix (lineno $1, Id s)
                     _ -> error "Parse error: bad id" }
      | int      { case unTok $1 of
                     TInt i -> AnnFix (lineno $1, CInt i)
                     _ -> error "Parse error: bad int" }
      | string   { case unTok $1 of
                     TString s -> AnnFix (lineno $1, CString s)
                     _ -> error "Parse error: bad string" }
      | true     { case unTok $1 of
                     TBool True -> AnnFix (lineno $1, CBool True)
                     _ -> error "Parse error: bad true" }
      | false    { case unTok $1 of
                     TBool False -> AnnFix (lineno $1, CBool False)
                     _ -> error "Parse error: bad false" }
      | Expr '+' Expr           { AnnFix (lineno $2, Add $1 $3) }
      | Expr '-' Expr           { AnnFix (lineno $2, Add $1 $3) }
      | Expr '*' Expr           { AnnFix (lineno $2, Add $1 $3) }
      | Expr '/' Expr           { AnnFix (lineno $2, Add $1 $3) }
      | '~' Expr                { AnnFix (lineno $1, Neg $2) }
      | Expr '<' Expr           { AnnFix (lineno $2, Lt $1 $3) }
      | Expr '==' Expr          { AnnFix (lineno $2, Eq $1 $3) }
      | Expr '<=' Expr          { AnnFix (lineno $2, Le $1 $3) }
      | Expr '&&' Expr          { AnnFix (lineno $2, And $1 $3) }
      | Expr '||' Expr          { AnnFix (lineno $2, Or $1 $3) }
      | not Expr              { AnnFix (lineno $1, Not $2) }
      | '(' ExprCommaList ')'   { AnnFix (lineno $1, Tuple $2) }
      | '(' ')'                 { AnnFix (lineno $1, Unit) }
      | Expr ':' Expr           { AnnFix (lineno $2, Cons $1 $3) }
      | '[' ']'                 { AnnFix (lineno $1, EmptyList) }
      | let id '=' Expr in Expr { AnnFix (lineno $1,
                                          case unTok $2 of
                                            TId s -> Let s $4 $6
                                            _ -> error "Parse error: bad let") }
      | case Expr of CaseList   { AnnFix (lineno $1, Case $2 $4) }
      | if Expr then Expr else Expr { AnnFix (lineno $1,
                                              If $2 $4 $6) }
      | Expr Expr %prec APP   { case $1 of
                                  AnnFix (l, _) ->
                                    AnnFix (l, App $1 $2) }
      | type ExprList   { AnnFix (lineno $1,
                                  case unTok $1 of
                                    TType s -> Constructor s $2
                                    _ -> error "Parse error: bad constructor") }

ExprCommaList :: { [PosExpr] }
               : Expr                     { [$1] }
               | ExprCommaList ',' Expr   { $1 ++ [$3] }

CaseList :: { [(PExpr, PosExpr)] }
          : CaseBranch            { [$1] }
          | CaseList CaseBranch   { $1 ++ [$2] }

CaseBranch :: { (PExpr, PosExpr) }
            : PExpr '->' Expr   { ($1, $3) }

ExprList :: { [PosExpr] }
          : {- empty -}     { [] }
          | ExprList Expr   { $1 ++ [$2] }

Datatype :: { Datatype }
          : data type '=' ConstructorList { case $2 of
                                              Token (TType s) l ->
                                                Datatype { typeName = s
                                                         , constructors = $4
                                                         , typeLine = l }
                                              _ -> error "Parse error: bad constructor"}

ConstructorList :: { [(String, [Type])] }
                 : Constructor                     { [$1] }
                 | ConstructorList '|' Constructor { $3 : $1 }

Constructor :: { (String, [Type]) }
             : type TypeList { case unTok $1 of
                                 TType s -> (s, $2)
                                 _ -> error "Parse error: bad type list" }

TypeList :: { [Type] }
          : {- empty -}        { [] }
          | TypeList FunType   { $1 ++ [$2] }

{

mkFunction :: Token -> Type -> [([PExpr], PosExpr)] -> PosFunction
mkFunction (Token (TId name) line) t body =
  Function { funName = name
           , funType = t
           , funDef = body
           , funLine = line }
mkFunction _ _ _ = error "Parse error: bad function declaration"

mkPExpr :: Token -> PExpr
mkPExpr (Token (TId s)     _) = PId s
mkPExpr (Token (TInt i)    _) = PInt i
mkPExpr (Token (TString s) _) = PString s
mkPExpr (Token (TBool b)   _) = PBool b
mkPExpr _ = error "Parser error: bad argument to mkPExpr"

parseError :: Token -> P a
parseError t = do
  lno <- getLineNo
  error $ "Parse error on line " ++ show lno ++ " on token " ++ show t

parse :: String -> PosAST
parse = evalP parserAct

}
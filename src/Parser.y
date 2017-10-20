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

';'      { Token TSemi _ }
'::'     { Token TDoubleColon _ }
'('      { Token TLParen _ }
')'      { Token TRParen _ }
'->'     { Token TSingleArrow _ }
id       { Token (TId _) _ }
'='      { Token TEquals _ }
'|'      { Token TBar _ }
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
'.'      { Token TDot _ }

%right '=>'
%left '&&'
%left '||'
%right not
%nonassoc '<' '==' '<='
%right ':'
%left '+' '-'
%left '*' '/'
%left '.'
%right '~'

%%

Program :: { PosAST }
         : DeclList   { AST $1 }

DeclList :: { [PosDeclaration] }
          : Declaration ';'            { [$1] }
          | DeclList Declaration ';'   { $2 : $1 }

Declaration :: { PosDeclaration }
             : FunDecl    { DFunDecl $1 }
             | FunDef     { DFunDef $1 }
             | Datatype   { DTypeDef $1 }

FunDecl :: { FunDecl }
         : id '::' Type   { case unTok $1 of
                              TId n -> FunDecl { funDeclName = n
                                               , funType = $3
                                               , funDeclLine = lineno $1 } }

Type :: { Type }
      : type                    { case unTok $1 of
                                    TType s -> BaseType s
                                    _ -> error "Parser error: bad type" }
      | id                      { case unTok $1 of
                                    TId s -> TypeVar s
                                    _ -> error "Parser error: bad type" }
      | '[' Type ']'            { ListType $2 }
      | '(' ')'                 { UnitType }
      | '(' Type ')'            { $2 }
      | '(' TypeCommaList ')'   { TupleType $2 }
      | Type '->' Type          { FunctionType $1 $3 }

TypeCommaList :: { [Type] }
               : Type ',' Type            { [$1, $3] }
               | TypeCommaList ',' Type   { $1 ++ [$3] }

FunDef :: { PosFunDef }
        : id PExprList '=' Expr { case unTok $1 of
                                    TId n -> FunDef { funDefName = n
                                                    , funDef = ($2, $4)
                                                    , funDefLine = lineno $1 } }

PExprList :: { [PExpr] }
           : {- empty -}       { [] }
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
       | type                     { case unTok $1 of
                                      TType s -> PConstructor s []
                                      _ -> error "Parse error: bad constructor" }
       | '(' type PExprList ')'   { case unTok $2 of
                                      TType s -> PConstructor s $3
                                      _ -> error "Parse error: bad constructor" }

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
      | Expr '-' Expr           { AnnFix (lineno $2, Sub $1 $3) }
      | Expr '*' Expr           { AnnFix (lineno $2, Mul $1 $3) }
      | Expr '/' Expr           { AnnFix (lineno $2, Div $1 $3) }
      | '~' Expr                { AnnFix (lineno $1, Neg $2) }
      | Expr '<' Expr           { AnnFix (lineno $2, Lt $1 $3) }
      | Expr '==' Expr          { AnnFix (lineno $2, Eq $1 $3) }
      | Expr '<=' Expr          { AnnFix (lineno $2, Le $1 $3) }
      | Expr '&&' Expr          { AnnFix (lineno $2, And $1 $3) }
      | Expr '||' Expr          { AnnFix (lineno $2, Or $1 $3) }
      | not Expr                { AnnFix (lineno $1, Not $2) }
      | Expr '.' Expr           { AnnFix (lineno $2, App $1 $3) }
      | Application             { $1 }
      | '(' ExprCommaList ')'   { AnnFix (lineno $1, Tuple $2) }
      | '(' Expr ')'            { $2 }
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

Application :: { PosExpr }
             : AExpr                { $1 }
             | Application AExpr    { AnnFix (exprLine $1, App $1 $2) }

AExpr :: { PosExpr }
      : id       { case unTok $1 of
                     TId s -> AnnFix (lineno $1, Id s)
                     _ -> error "Parse error: bad id" }
      | type     { case unTok $1 of
                     TType s -> AnnFix (lineno $1, Id s) }
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
       | '(' ')'                 { AnnFix (lineno $1, Unit) }
       | '(' Expr ')'            { $2 }
       | '(' ExprCommaList ')'   { AnnFix (lineno $1, Tuple $2) }
       | '[' ']'                 { AnnFix (lineno $1, EmptyList) }

ExprCommaList :: { [PosExpr] }
               : Expr ',' Expr            { [$1, $3] }
               | ExprCommaList ',' Expr   { $1 ++ [$3] }

CaseList :: { [(PExpr, PosExpr)] }
          : CaseBranch ';'           { [$1] }
          | CaseList CaseBranch ';'  { $1 ++ [$2] }

CaseBranch :: { (PExpr, PosExpr) }
            : PExpr '->' Expr   { ($1, $3) }

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
          : {- empty -}     { [] }
          | TypeList Type   { $1 ++ [$2] }

{

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

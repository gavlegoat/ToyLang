{-# LANGUAGE FlexibleInstances #-}

module Types where

-- Expressions allowed for pattern matching
data PExpr =
    PId String
  | PInt Int
  | PString String
  | PBool Bool
  | PUnit
  | PTuple [PExpr]
  | PEmptyList
  | PCons PExpr PExpr
  | PConstructor String [PExpr]
  deriving (Show)

data ExprF a =
    Id String
  | CInt Int
  | CString String
  | CBool Bool
  | Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Neg a
  | Lt a a
  | Eq a a
  | Le a a
  | And a a
  | Or a a
  | Not a
  | Tuple [a]
  | Unit
  | Cons a a
  | EmptyList
  | Let String a a
  | Case a [(PExpr, a)]
  | If a a a
  | App a a
  | Constructor String [a]
  | Internal String
  deriving (Show)

instance Functor ExprF where
  fmap f (Id s) = Id s
  fmap f (CInt i) = CInt i
  fmap f (CString s) = CString s
  fmap f (CBool b) = CBool b
  fmap f (Add a b) = Add (f a) (f b)
  fmap f (Sub a b) = Sub (f a) (f b)
  fmap f (Mul a b) = Mul (f a) (f b)
  fmap f (Div a b) = Div (f a) (f b)
  fmap f (Neg a) = Neg (f a)
  fmap f (Lt a b) = Lt (f a) (f b)
  fmap f (Eq a b) = Eq (f a) (f b)
  fmap f (Le a b) = Le (f a) (f b)
  fmap f (And a b) = And (f a) (f b)
  fmap f (Or a b) = Or (f a) (f b)
  fmap f (Not a) = Not (f a)
  fmap f (Tuple l) = Tuple (map f l)
  fmap f Unit = Unit
  fmap f (Cons a b) = Cons (f a) (f b)
  fmap f (EmptyList) = EmptyList
  fmap f (Let s a b) = Let s (f a) (f b)
  fmap f (Case e bs) = Case (f e) (zip (map fst bs) (map (f . snd) bs))
  fmap f (If a b c) = If (f a) (f b) (f c)
  fmap f (App a b) = App (f a) (f b)
  fmap f (Constructor s l) = Constructor s (map f l)
  fmap f (Internal s) = Internal s

-- A functions type may either be a single type or a function which takes
-- one argument and returns a result (functions are curried)
data Type = BaseType String
          | ListType Type
          | UnitType
          | TupleType [Type]
          | FunctionType Type Type
          deriving (Eq, Show)

newtype AnnFix x f = AnnFix { unAnnFix :: (x, f (AnnFix x f)) }

instance (Show a) => Show (AnnFix a ExprF) where
  show (AnnFix (x, expr)) = "(" ++ show x ++ ": " ++ showExpr expr ++ ")"

showExpr :: (Show a) => ExprF a -> String
showExpr expr = case expr of
  Id s            -> "Id " ++ s
  CInt i          -> "Cint " ++ show i
  CString s       -> "CString " ++ s
  CBool b         -> "CBool " ++ show b
  Add a b         -> "Add " ++ show a ++ " " ++ show b
  Sub a b         -> "Sub " ++ show a ++ " " ++ show b
  Mul a b         -> "Mul " ++ show a ++ " " ++ show b
  Div a b         -> "Div " ++ show a ++ " " ++ show b
  Neg a           -> "Neg" ++ show a
  Lt a b          -> "Lt " ++ show a ++ " " ++ show b
  Eq a b          -> "Eq " ++ show a ++ " " ++ show b
  Le a b          -> "Le " ++ show a ++ " " ++ show b
  And a b         -> "And " ++ show a ++ " " ++ show b
  Or a b          -> "Or " ++ show a ++ " " ++ show b
  Not a           -> "Not " ++ show a
  Tuple l         -> "Tuple " ++ show l
  Unit            -> "Unit"
  Cons a b        -> "Cons " ++ show a ++ " " ++ show b
  EmptyList       -> "EmptyList"
  Let s a b       -> "Let " ++ s ++ " " ++ show a ++ " " ++ show b
  Case a cs       -> "Case " ++ show a ++ " " ++ show cs
  If i t e        -> "If " ++ show i ++ " " ++ show t ++ " " ++ show e
  App a b         -> "App " ++ show a ++ " " ++ show b
  Constructor s l -> "Constructor " ++ show s ++ " " ++ show l
  Internal s      -> "Internal " ++ s

-- The declaration is the type signature for a function
data FunDecl = FunDecl { funDeclName :: String
                       , funType :: Type
                       , funDeclLine :: Int}
                       deriving (Show)

-- The definition includes a list of expressions to pattern match on and the namd
data FunDef a = FunDef { funDefName :: String
                       , funDef :: ([PExpr], AnnFix a ExprF)
                       , funDefLine :: Int }
                       deriving (Show)

-- A datatype has a name and a number of constructors, each of which has a
-- name and a number of fields
data Datatype = Datatype { typeName :: String
                         , constructors :: [(String, [Type])]
                         , typeLine :: Int }
                         deriving (Show)

data Declaration a = DFunDecl FunDecl
                   | DFunDef (FunDef a)
                   | DTypeDef Datatype
                   deriving (Show)

newtype AST a = AST [Declaration a]
  deriving (Show)

type PosExpr = AnnFix Int ExprF

exprLine :: PosExpr -> Int
exprLine (AnnFix (l, _)) = l

type PosFunDef = FunDef Int
type PosDeclaration = Declaration Int
type PosAST = AST Int

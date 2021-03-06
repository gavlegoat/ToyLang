module Typecheck where

import Control.Monad
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import Data.Maybe

import Types

data Exception = Exception { errLine :: Int, errMsg :: String }

-- This is a state which will be carried through typechecking
-- env should include both function and constructor declarations
data TypecheckState = TypecheckState { env :: Map String Type
                                     , typecheckErrors :: [Exception] }

-- Replace all occurances of 'a' with 'b' in the type 't'
substType :: Type -> Type -> Type -> Type
substType t a b =
  if t == a then b else case t of
    ListType t' -> ListType (substType t' a b)
    TupleType ts -> TupleType $ map (\x -> substType x a b) ts
    FunctionType t1 t2 -> FunctionType (substType t1 a b) (substType t2 a b)
    _ -> t

-- Since +, -, *, /, <, <=, and, or are all very similar, we use this function
-- to typecheck any of those operators
annotateBinOp :: PosExpr -> PosExpr -> Type -> Type -> Type -> Int ->
                 String -> State TypecheckState (Maybe TypeExpr)
annotateBinOp a b t1 t2 t3 l errStr = do
  m1 <- annotateExpr a
  m2 <- annotateExpr b
  case (m1, m2) of
    (Just x, Just y) ->
      if exprType x == t1 && exprType y == t2
      then return . Just $ AnnFix (TypeAnn l t3, Add x y)
      else do
        st <- get
        put $ st { typecheckErrors = Exception { errLine = l
                                               , errMsg = errStr }
                                      : typecheckErrors st}
        return Nothing
    _ -> return Nothing

-- Add a type to an expression
annotateExpr :: PosExpr -> State TypecheckState (Maybe TypeExpr)
annotateExpr (AnnFix (l, e)) = case e of
  Id s -> do
    st <- get
    case Map.lookup s (env st) of
      Nothing -> do
        put (st { typecheckErrors = Exception {
            errLine = l
          , errMsg = "Variable not found: " ++ s } : typecheckErrors st })
        return Nothing
      Just t -> return . Just $ AnnFix (TypeAnn l t, Id s)
  CInt i    -> return . Just $ AnnFix (TypeAnn l (BaseType "Int"), CInt i)
  CString s -> return . Just $ AnnFix (TypeAnn l (BaseType "String"), CString s)
  CBool b   -> return . Just $ AnnFix (TypeAnn l (BaseType "Bool"), CBool b)
  Add a b -> annotateBinOp a b (BaseType "Int") (BaseType "Int") (BaseType "Int") l "+ applied to non-integers"
  Sub a b -> annotateBinOp a b (BaseType "Int") (BaseType "Int") (BaseType "Int") l "- applied to non-integers"
  Mul a b -> annotateBinOp a b (BaseType "Int") (BaseType "Int") (BaseType "Int") l "* applied to non-integers"
  Div a b -> annotateBinOp a b (BaseType "Int") (BaseType "Int") (BaseType "Int") l "/ applied to non-integers"
  Neg a -> do
    m <- annotateExpr a
    case m of
      Just x -> if exprType x == BaseType "Int"
                then return . Just $ AnnFix (TypeAnn l (BaseType "Int"), Neg x)
                else do
                  st <- get
                  put $ st { typecheckErrors = Exception { errLine = l
                                                         , errMsg = "~ applied to noninteger" }
                                               : typecheckErrors st }
                  return Nothing
      _ -> return Nothing
  Lt a b -> annotateBinOp a b (BaseType "Int") (BaseType "Int") (BaseType "Bool") l "< applied to non-integers"
  Eq a b -> do
    m1 <- annotateExpr a
    m2 <- annotateExpr b
    case (m1, m2) of
      (Just x, Just y) ->
        case (exprType x, exprType y) of
          (BaseType s1, BaseType s2) ->
            if s1 == s2
            then return . Just $ AnnFix (TypeAnn l (BaseType "Bool"), Eq x y)
            else do
              st <- get
              put $ st { typecheckErrors = Exception { errLine = l
                                                     , errMsg = "= applied different types" }
                                           : typecheckErrors st }
              return Nothing
          _ -> do
            st <- get
            put $ st { typecheckErrors = Exception { errLine = l
                                                   , errMsg = "= applied to non-basic types" }
                                         : typecheckErrors st }
            return Nothing
      _ -> return Nothing
  Le a b -> annotateBinOp a b (BaseType "Int") (BaseType "Int") (BaseType "Bool") l "<= applied to non-integers"
  And a b -> annotateBinOp a b (BaseType "Bool") (BaseType "Bool") (BaseType "Bool") l "'and' applied to non-booleans"
  Or a b -> annotateBinOp a b (BaseType "Bool") (BaseType "Bool") (BaseType "Bool") l "'or' applied to non-booleans"
  Not a -> do
    m <- annotateExpr a
    case m of
      Just x ->
        if exprType x == BaseType "Bool"
        then return . Just $ AnnFix (TypeAnn l (BaseType "Bool"), Not x)
        else do
          st <- get
          put $ st { typecheckErrors = Exception { errLine = l
                                                 , errMsg = "'not' applied to nonboolean" }
                                       : typecheckErrors st }
          return Nothing
      _ -> return Nothing
  Tuple t -> do
    s <- mapM annotateExpr t
    if any isNothing s
      then return Nothing
      else let ss = catMaybes s
               types = map exprType ss
           in return . Just $ AnnFix (TypeAnn l (TupleType types), Tuple ss)
  Unit -> return . Just $ AnnFix (TypeAnn l UnitType, Unit)
  Cons a b -> do
    m1 <- annotateExpr a
    m2 <- annotateExpr b
    case (m1, m2) of
      (Just x, Just y) -> case exprType y of
        ListType y' -> case y' of
          TypeVar _ -> return . Just $ AnnFix (TypeAnn l (ListType (exprType x)),
                                               Cons x y)
          _ -> if exprType x == y'
               then return . Just $ AnnFix (TypeANn l (ListType y'), Cons x y)
               else do
                 st <- get
                 put $ st { typecheckErrrors = Exception { errLine = l
                                                         , errMsg = "Mismatched arguments to :" }
                                               : typecheckErrors st }
                 return Nothing
        _ -> do
          st <- get
          put $ st { typecheckErrors = Exception { errLine = l
                                                 , errMsg = "Second argument to : is not a list" }
                                       : typecheckErrors st }
          return Nothing
      _ -> return Nothing
  EmptyList -> return . Just $ AnnFix (TypeAnn l (ListType (TypeVar "emptyListType")), EmptyList)
  Let s a b -> do
    m1 <- annotateExpr a
    case m1 of
      Just x -> do
        st <- get
        oldEnv <- env st
        put $ st { env = Map.insert s (exprType x) env }
        m2 <- annotateExpr b
        put $ st { env = oldEnv }
        return m2
  Case a cs -> undefined
  If i t e -> do
    m1 <- annotateExpr i
    m2 <- annotateExpr t
    m3 <- annotateExpr e
    case (m1, m2, m3) of
      (Just x, Just y, Just z) -> case exprType x of
        BaseType "Bool" -> case (exprType y, exprType z) of
          (TypeVar _, TypeVar _) -> return . Just $ AnnFix (TypeAnn l (exprType y), If x y z)
          _ ->
            if exprType y == exprType z
            then return . Just $ AnnFix (TypeAnn l (exprType y), If x y z)
            else do
              st <- get
              put $ st { typecheckErrors = Exception { errLine = l
                                                     , errMsg = "Type mismatch in if branches" }
                                           : typecheckErrors st }
              return Nothing
        _ -> do
          st <- get
          put $ st { typecheckErrors = Exception { errLine = l
                                                 , errMsg = "If test is not boolean" }
                                       : typecheckErrors st }
          return Nothing
      _ -> return Nothing
  App a b -> do
    m1 <- annotateExpr a
    m2 <- annotateExpr b
    case (m1, m2) of
      (Just x, Just y) -> case x of
        FunctionType t1 t2 -> case t1 of
          TypeVar s -> return . Just $ AnnFix (TypeAnn l (substType t2 t1 (exprType y)),
                                               App x y)
        _ -> do
          st <- get
          put $ st { typecheckErrors = Exception { errLine = l
                                                 , errMsg = "Non-function is applied to argument" }
                                       : typecheckErrors st }
          return Nothing
  Internal s -> case s of
    "concat" -> return . Just $ AnnFix (TypeAnn l (FunctionType
                                                     (BaseType "String")
                                                     (FunctionType
                                                        (BaseType "String")
                                                        (BaseType "String"))),
                                        Internal "concat")
    "substr" -> return . Just $ AnnFix (TypeAnn l (FunctionType
                                                     (BaseType "String")
                                                     (FunctionType
                                                        (BaseType "Int")
                                                        (FunctionType
                                                           (BaseType "Int")
                                                           (BaseType "String")))),
                                        Internal "substr")
    "length" -> return . Just $ AnnFix (TypeAnn l (FunctionType
                                                     (BaseType "String")
                                                     (BaseType "Int")),
                                        Internal "length")
    "print" -> return . Just $ AnnFix (TypeAnn l (FunctionType
                                                    (TypeVar "a")
                                                    UnitType),
                                       Internal "print")
    "error" -> return . Just $ AnnFix (TypeAnn l (FunctionType
                                                    (BaseType "string")
                                                    UnitType),
                                       Internal "error")

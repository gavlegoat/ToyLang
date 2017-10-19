module Typecheck where

import Control.Monad
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import Data.Maybe

import Types

data Exception = Exception { errLine :: Int, errMsg :: String }

-- This is a state which will be carried through typechecking
data TypecheckState = TypecheckState { env :: Map String Type
                                     , funDecls :: [FunDecl]
                                     , typecheckErrors :: [Exception] }

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
  Add a b -> do
    m1 <- annotateExpr a
    m2 <- annotateExpr b
    case (m1, m2) of
      (Just x, Just y) ->
        if (exprType x == BaseType "Int" && exprType y == BaseType "Int")
        then return . Just $ AnnFix (TypeAnn l (BaseType "Int"), Add x y)
        else do
          st <- get
          put $ st { typecheckErrors = Exception { errLine = l
                                                 , errMsg = "+ applied to nonintegers" }
                                        : typecheckErrors st}
          return Nothing
      _ -> return Nothing
  Sub a b -> do
    m1 <- annotateExpr a
    m2 <- annotateExpr b
    case (m1, m2) of
      (Just x, Just y) ->
        if (exprType x == BaseType "Int" && exprType y == BaseType "Int")
        then return . Just $ AnnFix (TypeAnn l (BaseType "Int"), Sub x y)
        else do
          st <- get
          put $ st { typecheckErrors = Exception { errLine = l
                                                 , errMsg = "- applied to nonintegers" }
                                       : typecheckErrors st }
          return Nothing
      _ -> return Nothing
  Mul a b -> do
    m1 <- annotateExpr a
    m2 <- annotateExpr b
    case (m1, m2) of
      (Just x, Just y) ->
        if (exprType x == BaseType "Int" && exprType y == BaseType "Int")
        then return . Just $ AnnFix (TypeAnn l (BaseType "Int"), Mul x y)
        else do
          st <- get
          put $ st { typecheckErrors = Exception { errLine = l
                                                 , errMsg = "* applied to nonintegers" }
                                       : typecheckErrors st }
          return Nothing
      _ -> return Nothing
  Div a b -> do
    m1 <- annotateExpr a
    m2 <- annotateExpr b
    case (m1, m2) of
      (Just x, Just y) ->
        if (exprType x == BaseType "Int" && exprType y == BaseType "Int")
        then return . Just $ AnnFix (TypeAnn l (BaseType "Int"), Div x y)
        else do
          st <- get
          put $ st { typecheckErrors = Exception { errLine = l
                                                 , errMsg = "/ applied to nonintegers" }
                                       : typecheckErrors st }
          return Nothing
      _ -> return Nothing
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
  Lt a b -> do
    m1 <- annotateExpr a
    m2 <- annotateExpr b
    case (m1, m2) of
      (Just x, Just y) ->
        if (exprType x == BaseType "Int" && exprType y == BaseType "Int")
        then return . Just $ AnnFix (TypeAnn l (BaseType "Bool"), Lt x y)
        else do
          st <- get
          put $ st { typecheckErrors = Exception { errLine = l
                                                 , errMsg = "< applied to nonintegers" }
                                       : typecheckErrors st }
          return Nothing
      _ -> return Nothing
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
  Le a b -> do
    m1 <- annotateExpr a
    m2 <- annotateExpr b
    case (m1, m2) of
      (Just x, Just y) ->
        if (exprType x == BaseType "Int" && exprType y == BaseType "Int")
        then return . Just $ AnnFix (TypeAnn l (BaseType "Bool"), Le x y)
        else do
          st <- get
          put $ st { typecheckErrors = Exception { errLine = l
                                                 , errMsg = "<= applied to nonintegers" }
                                       : typecheckErrors st }
          return Nothing
      _ -> return Nothing
  And a b -> do
    m1 <- annotateExpr a
    m2 <- annotateExpr b
    case (m1, m2) of
      (Just x, Just y) ->
        if (exprType x == BaseType "Bool" && exprType y == BaseType "Bool")
        then return . Just $ AnnFix (TypeAnn l (BaseType "Bool"), And x y)
        else do
          st <- get
          put $ st { typecheckErrors = Exception { errLine = l
                                                 , errMsg = "'and' applied to nonbooleans" }
                                       : typecheckErrors st }
          return Nothing
      _ -> return Nothing
  Or a b -> do
    m1 <- annotateExpr a
    m2 <- annotateExpr b
    case (m1, m2) of
      (Just x, Just y) ->
        if (exprType x == BaseType "Bool" && exprType y == BaseType "Bool")
        then return . Just $ AnnFix (TypeAnn l (BaseType "Bool"), Or x y)
        else do
          st <- get
          put $ st { typecheckErrors = Exception { errLine = l
                                                 , errMsg = "'or' applied to nonbooleans" }
                                       : typecheckErrors st }
          return Nothing
      _ -> return Nothing
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
  Cons a b -> undefined   -- Cons will need to deal with EmptyList having type (ListType UnitType)
  EmptyList -> return . Just $ AnnFix (TypeAnn l (ListType UnitType), EmptyList)
  Let s a b -> undefined
  Case a cs -> undefined
  If i t e -> do
    m1 <- annotateExpr i
    m2 <- annotateExpr t
    m3 <- annotateExpr e
    case (m1, m2, m3) of
      (Just x, Just y, Just z) -> case exprType x of
        BaseType "Bool" ->
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
      (Just x, Just y) -> case exprType x of
        FunctionType t1 t2 ->
          if exprType y == t1
          then return . Just $ AnnFix (TypeAnn l t2, App x y)
          else do
            st <- get
            put $ st { typecheckErrors = Exception { errLine = l
                                                   , errMsg = "Function applied to bad argument types" }
                                         : typecheckErrors st }
            return Nothing
        _ -> do
          st <- get
          put $ st { typecheckErrors = Exception { errLine = l
                                                 , errMsg = "Non-function applied to argument" }
                                       : typecheckErrors st }
          return Nothing
      _ -> return Nothing
  Constructor s es -> undefined
module Typecheck where

import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Map (Map)

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
                                                 , errMsg = "* applied to nonintegers"}
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
                                                 , errMsg = "/ applied to nonintegers"}
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
                  put $ st { typecheckErrors = Exception {
                      errLine = l
                    , errMsg = "~ applied to noninteger"}}
                  return Nothing
      _ -> return Nothing
  Lt a b -> undefined
  Eq a b -> undefined
  Le a b -> undefined
  And a b -> undefined
  Or a b -> undefined
  Not a -> undefined
  Tuple l -> undefined
  Unit -> return . Just $ AnnFix (TypeAnn l UnitType, Unit)
  Cons a b -> undefined   -- Cons will need to deal with EmptyList having type (ListType UnitType)
  EmptyList -> return . Just $ AnnFix (TypeAnn l (ListType UnitType), EmptyList)
  Let s a b -> undefined
  Case a cs -> undefined
  If i t e -> undefined
  App a b -> undefined
  Constructor s l -> undefined
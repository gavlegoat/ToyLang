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

addType :: PosExpr -> State TypecheckState (Maybe TypeExpr)
addType (AnnFix (l, e)) = case e of
  Id s            -> do
    st <- get
    case Map.lookup s (env st) of
      Nothing -> do
        put (st { typecheckErrors = Exception {
            errLine = l
          , errMsg = "Variable not found: " ++ s } : typecheckErrors st })
        return Nothing
      Just t -> return . Just $ AnnFix (TypeAnn l t, Id s)
  CInt i          -> undefined
  CString s       -> undefined
  CBool b         -> undefined
  Add a b         -> undefined
  Sub a b         -> undefined
  Mul a b         -> undefined
  Div a b         -> undefined
  Neg a           -> undefined
  Lt a b          -> undefined
  Eq a b          -> undefined
  Le a b          -> undefined
  And a b         -> undefined
  Or a b          -> undefined
  Not a           -> undefined
  Tuple l         -> undefined
  Unit            -> undefined
  Cons a b        -> undefined
  EmptyList       -> undefined
  Let s a b       -> undefined
  Case a cs       -> undefined
  If i t e        -> undefined
  App a b         -> undefined
  Constructor s l -> undefined
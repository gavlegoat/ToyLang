module Typecheck where

import Control.Monad.State
import Data.Map

import Types

-- This is a state which will be carried through typechecking
data TypecheckState = TypecheckState { env :: Map String String }


addType :: PosExpr -> State TypecheckEnv TypeExpr
addType (AnnFix (l, e)) = case e of
  Id s            ->
  CInt i          ->
  CString s       ->
  CBool b         ->
  Add a b         ->
  Sub a b         ->
  Mul a b         ->
  Div a b         ->
  Neg a           ->
  Lt a b          ->
  Eq a b          ->
  Le a b          ->
  And a b         ->
  Or a b          ->
  Not a           ->
  Tuple l         ->
  Unit            ->
  Cons a b        ->
  EmptyList       ->
  Let s a b       ->
  Case a cs       ->
  If i t e        ->
  App a b         ->
  Constructor s l ->
{-
This module performs a number of semantic analysis tasks, including checking
certain properties, such as:
  - A main function is defined
  - Every referenced type is defined
  - Each function declaration has an associated definition
  - No two declarations should have the same name
-}

module SemanticAnalysis (semanticChecks, collectPieces) where

import Data.List

import Types

builtInDecls :: [FunDecl]
builtInDecls = [
    FunDecl "concat"
            (FunctionType (BaseType "String")
                          (FunctionType (BaseType "String")
                                        (BaseType "String")))
            0
  , FunDecl "substr"
            (FunctionType (BaseType "String")
                          (FunctionType (BaseType "Int")
                                        (FunctionType (BaseType "Int")
                                                      (BaseType "String"))))
            0
  , FunDecl "length"
            (FunctionType (BaseType "String") (BaseType "Int"))
            0
  , FunDecl "error" (FunctionType (BaseType "String") UnitType) 0
  , FunDecl "printInt" (FunctionType (BaseType "Int") UnitType) 0
  , FunDecl "printString"
            (FunctionType (BaseType "String") UnitType)
            0
  , FunDecl "printBool"
            (FunctionType (BaseType "Bool") UnitType)
            0
  ]

builtInDefs :: [PosFunDef]
builtInDefs = [
    FunDef "concat"      ([], AnnFix (0, Internal "concat"     )) 0
  , FunDef "substr"      ([], AnnFix (0, Internal "substr"     )) 0
  , FunDef "length"      ([], AnnFix (0, Internal "length"     )) 0
  , FunDef "error"       ([], AnnFix (0, Internal "error"      )) 0
  , FunDef "print"       ([], AnnFix (0, Internal "printBool"  )) 0
  ]

builtInTypes :: [Datatype]
builtInTypes = [
    Datatype "Int"    [] 0
  , Datatype "String" [] 0
  , Datatype "Bool"   [] 0
  ]

collectFunDecls :: PosAST -> [FunDecl]
collectFunDecls (AST cs) = [ x | (DFunDecl x) <- cs ] ++ builtInDecls

collectFunDefs :: PosAST -> [PosFunDef]
collectFunDefs (AST cs) = [ x | (DFunDef x) <- cs ] ++ builtInDefs

collectTypeDefs :: PosAST -> [Datatype]
collectTypeDefs (AST cs) = [ x | (DTypeDef x) <- cs] ++ builtInTypes

data Exception = Exception { errLine :: Int, errMsg :: String }

instance Show Exception where
  show e = "Semantic error on line " ++ show (errLine e) ++ ": " ++ errMsg e

type SemanticCheck = ([FunDecl], [PosFunDef], [Datatype]) -> [Exception]

-- Ensure there is a main function declared
checkMainDecl :: SemanticCheck
checkMainDecl (ds, _, _) =
  let l = filter ((== "main") . funDeclName) ds in
  case l of
    [d] -> if funType d == UnitType
           then []
           else [Exception { errLine = funDeclLine d
                           , errMsg = "Expected main function to have type ()"}]
    _ -> [Exception { errLine = 0, errMsg = "No main function found" }]

-- Ensure each declaration has an associated definition
defsExist :: SemanticCheck
defsExist (decls, defs, _) = concatMap defExists decls where
  defExists d = case filter ((== funDeclName d) . funDefName) defs of
                  [] -> [Exception { errLine = funDeclLine d
                                   , errMsg = "Function " ++ funDeclName d ++
                                              " lacks a definition"}]
                  _ -> []

-- Ensure the type t exists. types is a list of all types
typesExist :: [Datatype] -> Int -> Type -> [Exception]
typesExist types l t = case t of
  BaseType s -> if any ((== s) . typeName) types
                then []
                else [Exception { errLine = l
                                , errMsg = "Type " ++ s ++ " not found"}]
  ListType t' -> typesExist types l t'
  UnitType -> []
  TupleType ts -> concatMap (typesExist types l) ts
  FunctionType t1 t2 -> typesExist types l t1 ++ typesExist types l t2
  TypeVar _ -> []

-- Ensure any type showing up in a function signature is defined somewhere
funTypesExist :: SemanticCheck
funTypesExist (decls, _, types) = concatMap exist' decls where
  exist' d = typesExist types (funDeclLine d) $ funType d

-- Ensure any type showing up in a datatype definition is defined somewhere.
-- Note that types may be recursive or mutually recursive.
typeTypesExist :: SemanticCheck
typeTypesExist (_, _, types) = concatMap exist' types where
  exist' t = concatMap (concatMap (typesExist types (typeLine t)) . snd) $
                       constructors t

-- Ensure no two function declarations have the same name
funDuplicates :: SemanticCheck
funDuplicates (decls, _, _) = concatMap duplicate decls where
  duplicate d =
    case filter ((== funDeclName d) . funDeclName) decls of
      [_] -> []
      l -> [Exception { errLine = funDeclLine d
                      , errMsg = "Function " ++ funDeclName d ++
                                 " redeclared. Declarations at " ++
                                 intercalate "," (map (show . funDeclLine) l) }]

-- Ensure no two datatypes have the same name
typeDuplicates :: SemanticCheck
typeDuplicates (_, _, types) = concatMap duplicate types where
  duplicate t =
    case filter ((== typeName t) . typeName) types of
      [_] -> []
      l -> [Exception { errLine = typeLine t
                      , errMsg = "Type " ++ typeName t ++ " redefined. " ++
                                 "Definitions at " ++
                                 intercalate "," (map (show . typeLine) l) }]

collectPieces :: PosAST -> ([FunDecl], [PosFunDef], [Datatype])
collectPieces ast = (collectFunDecls ast,
                     collectFunDefs ast,
                     collectTypeDefs ast)

checkList :: [SemanticCheck]
checkList = [ checkMainDecl
            , defsExist
            , funTypesExist
            , typeTypesExist
            , funDuplicates
            , typeDuplicates
            ]

semanticChecks :: ([FunDecl], [PosFunDef], [Datatype]) -> [Exception]
semanticChecks t = concat $ checkList <*> [t]

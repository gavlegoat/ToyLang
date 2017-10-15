{-
Perform a number of semantic checks on the code. These include
  - There is a main function
  - Within each declaration, the function names all match
  - No two declarations have the same name
  - Types in function signatures and definitions are defined
  - Matching does not bind the same name more than once
  - There is no cycle in the datatype definitions
-}
module SemanticChecks where

newtype Exception = Exception { errLine :: Int, errMsg :: String }

mainExists :: PosAST -> [Exception]
mainExists (AST cs) = any (case d of
                             FunDecl f -> funName f == "main"
                             _ -> False) cs



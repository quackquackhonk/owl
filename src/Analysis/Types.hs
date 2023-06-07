module Analysis.Types (typecheck) where

import Syntax.AST
import Eval.Environment (Environment)

-- | TODO: implement typechecking of OWL expressions
typecheck :: Environment -> Expression -> Maybe Type
typecheck env expr = Nothing

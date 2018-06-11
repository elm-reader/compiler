{-# LANGUAGE OverloadedStrings #-}

module Reader.Hooks
  ( recordExpr
  ) where


import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified AST.Canonical as Can
import qualified Elm.Name as N
import qualified AST.Module.Name as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- API SYMBOLS


symbol :: Text.Text -> [Text.Text] -> Can.Type -> Can.Expr
symbol name freeVars type_ =
  let
    freeVar var =
      (N.fromText var, ())

    freeVarsMap =
      Map.fromList $ map freeVar freeVars

    annot =
      Can.Forall freeVarsMap type_

    expr =
      Can.VarForeign ModuleName.reader (N.fromText name) annot
  in
    A.At R.zero expr


{-# NOINLINE recordExpr #-}
recordExpr :: Can.Expr
recordExpr =
  symbol "recordExpr" ["a"] $
  Can.TLambda (Can.TType ModuleName.basics N.int []) $
  Can.TLambda (Can.TVar $ N.fromText "a") $
  Can.TVar (N.fromText "a")

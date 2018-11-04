{-# LANGUAGE OverloadedStrings #-}

module Reader.Hooks
  ( recordExpr
  , recordCall
  , recordFrame
  , markInstrumented
  , seq
  ) where


import Prelude hiding (seq)
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
      Can.VarKernel (N.fromText "Reader") (N.fromText name)
  in
    A.At R.zero expr


{-# NOINLINE recordExpr #-}
recordExpr :: Can.Expr
recordExpr =
  symbol "recordExpr" ["a"] $
  Can.TLambda (Can.TType ModuleName.basics N.int []) $
  Can.TLambda (Can.TVar $ N.fromText "a") $
  Can.TVar (N.fromText "a")


{-# NOINLINE recordCall #-}
recordCall :: Can.Expr
recordCall =
  symbol "recordCall" ["a", "b", "c"] $
  Can.TLambda (Can.TType ModuleName.basics N.int []) $
  Can.TLambda (Can.TLambda (Can.TVar $ N.fromText "a") (Can.TVar $ N.fromText "b")) $
  Can.TLambda (Can.TLambda Can.TUnit (Can.TVar $ N.fromText "c")) $
  Can.TVar $ N.fromText "c"


{-# NOINLINE recordFrame #-}
recordFrame :: Can.Expr
recordFrame =
  symbol "recordFrame" ["a"] $
  Can.TLambda (Can.TType ModuleName.string N.string []) $
  Can.TLambda (Can.TLambda Can.TUnit (Can.TVar $ N.fromText "a")) $
  Can.TVar $ N.fromText "a"


{-# NOINLINE markInstrumented #-}
markInstrumented :: Can.Expr
markInstrumented =
  symbol "markInstrumented" ["a", "b"] $
  Can.TLambda (Can.TLambda (Can.TVar $ N.fromText "a") (Can.TVar $ N.fromText "b")) $
  Can.TLambda (Can.TVar $ N.fromText "a") (Can.TVar $ N.fromText "b")


{-# NOINLINE seq #-}
-- Semantically, `seq` is just `always identity`, but we want to implement it in kernel code to
-- avoid any possibility of a present or future function inliner beta-reducing our side effects into
-- oblivion.
seq :: Can.Expr
seq =
  symbol "seq" ["a", "b"] $
  Can.TLambda (Can.TVar $ N.fromText "a") $
  Can.TLambda (Can.TVar $ N.fromText "b") $
  Can.TVar (N.fromText "b")

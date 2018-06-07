{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reader.Instrument
  ( instrument
  )
  where


import Text.Show.Pretty (ppShow)
import Debug.Trace (trace)

import qualified Data.Map as Map
import Data.Text (pack)

import qualified AST.Canonical as Can
import qualified Reporting.Annotation as A
import qualified Reader.SourceMap as SrcMap



instrument :: Can.Module -> (Can.Module, SrcMap.Module)
instrument module_ =
  trace ("elm-reader debug -- received canonical AST:\n" ++ ppShow module_) $
    instrumentModule module_


instrumentModule :: Can.Module -> (Can.Module, SrcMap.Module)
instrumentModule module_ =
  (hookModule module_, SrcMap.Module Map.empty)

hookExpr :: Can.Expr -> Can.Expr
hookExpr expr@(A.At region expr_)  =
  case expr_ of
    Can.Call fn _args ->
      let
        readFn = A.At region (Can.VarKernel "Reader" "read")
      in
      A.At
        region
        (Can.Call readFn [A.At (A.toRegion fn) (Can.Str "a function"), A.At (A.toRegion fn) (Can.Str (pack $ unwords $ lines $ ppShow fn)), expr])
    _ -> expr

hookDef :: Can.Def -> Can.Def
hookDef (Can.Def n ps e) = Can.Def n ps (hookExpr e)
hookDef (Can.TypedDef n vs ps e t) = Can.TypedDef n vs ps (hookExpr e) t

hookDecls :: Can.Decls -> Can.Decls
hookDecls (Can.Declare def rest) = Can.Declare (hookDef def) (hookDecls rest)
-- hookDecls x = x
hookDecls (Can.DeclareRec defs rest) = Can.DeclareRec (map hookDef defs) (hookDecls rest)
hookDecls Can.SaveTheEnvironment = Can.SaveTheEnvironment

hookModule :: Can.Module -> Can.Module
hookModule m = m { Can._decls = hookDecls (Can._decls m) }

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reader.Instrument
  ( instrument
  )
  where


import Text.Show.Pretty (ppShow)
import Debug.Trace (trace)

import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)

import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R
import qualified Reader.SourceMap as SrcMap



instrument :: Can.Module -> (Can.Module, SrcMap.Module)
instrument module_ =
  let instrumented = instrumentModule module_
      message =
        "elm-reader debug -- received canonical AST:\n"
        ++ ppShow module_ ++ "\n"
        ++ "transformed to:\n"
        ++ ppShow instrumented
  in
  trace message instrumented


instrumentModule :: Can.Module -> (Can.Module, SrcMap.Module)
instrumentModule module_ =
  (hookModule module_, SrcMap.Module Map.empty)


readAt :: R.Region -> Can.Expr
readAt region =
  A.At region (Can.VarKernel "Reader" "read")


hookExpr :: Can.Expr -> Can.Expr
hookExpr (A.At region expr_)  =
  case expr_ of
    Can.Call fn _args ->
      let
        atFn = A.At (A.toRegion fn)
        funcName = atFn (Can.Str $ showText $ prettyPrintFunc $ A.toValue fn)
        range = atFn (Can.Str $ showText $ oneLineShow $ A.toRegion fn)
      in
      A.At
        region
        (Can.Call (readAt region) [funcName, range, A.At region expr_])

    Can.Binop opName _opModule _verbalName _type _op1 _op2 ->
      let
        locate = A.At region
        op = locate (Can.Str $ showText $ N.toText opName)
        range = locate (Can.Str $ showText $ oneLineShow $ A.toRegion op)
      in
      A.At
        region
        (Can.Call (readAt region) [op, range, A.At region expr_])

    _ ->
      A.At region expr_

hookDef :: Can.Def -> Can.Def
hookDef (Can.Def n ps e) = Can.Def n ps (hookExpr e)
hookDef (Can.TypedDef n vs ps e t) = Can.TypedDef n vs ps (hookExpr e) t

hookDecls :: Can.Decls -> Can.Decls
hookDecls (Can.Declare def rest) = Can.Declare (hookDef def) (hookDecls rest)
hookDecls (Can.DeclareRec defs rest) = Can.DeclareRec (map hookDef defs) (hookDecls rest)
hookDecls Can.SaveTheEnvironment = Can.SaveTheEnvironment

hookModule :: Can.Module -> Can.Module
hookModule m = m { Can._decls = hookDecls (Can._decls m) }


prettyPrintFunc :: Can.Expr_ -> Text
prettyPrintFunc expr =
  case expr of
    Can.VarLocal name ->
      N.toText name <> " (local)"

    Can.VarTopLevel modname name ->
      qualified modname name

    Can.VarKernel from name ->
      N.toText from <> "." <> N.toText name

    Can.VarForeign modname name _ ->
      qualified modname name

    ctor@(Can.VarCtor _ modname name _ _) ->
      qualified modname name <> "=[" <> oneLineShow ctor <> "]"

    var@(Can.VarDebug modname name _) ->
      qualified modname name <> "=[" <> oneLineShow var <> "]"

    x ->
      "[" <> oneLineShow x <> "]"

qualified :: ModuleName.Canonical -> N.Name -> Text
qualified modname name = showMod modname <> "." <> N.toText name
  where
    showMod (ModuleName.Canonical _package nm) = N.toText nm


oneLineShow :: (Show a) => a -> Text
oneLineShow = pack . unwords . lines . ppShow

showText :: Text -> Text
showText = pack . show . unpack

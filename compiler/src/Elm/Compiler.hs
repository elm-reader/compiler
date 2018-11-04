{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler
  ( version
  , Compile.DocsFlag(..)
  , Compile.ReaderFlag(..)
  , Compile.Instrumentation(..)
  , Compile.Artifacts(..)
  , compile
  , Error.Error
  , errorsToDoc
  , errorsToJson
  , Warning.Warning
  , consolidateSourceMaps
  )
  where


import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified AST.Module.Name as ModuleName
import qualified Compile
import qualified Elm.Compiler.Module as M
import qualified Elm.Compiler.Version
import qualified Elm.Package as Pkg
import qualified Json.Encode as Encode
import qualified Reader.SourceMap as SrcMap
import qualified Reporting.Doc as D
import qualified Reporting.Error as Error
import qualified Reporting.Render.Code as Code
import qualified Reporting.Region as Region
import qualified Reporting.Report as Report
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



-- VERSION


version :: Pkg.Version
version =
  Elm.Compiler.Version.version



-- COMPILE


compile
  :: Compile.DocsFlag
  -> Pkg.Name
  -> Map.Map M.Raw M.Canonical
  -> M.Interfaces
  -> BS.ByteString
  -> (Compile.ReaderFlag, Compile.Instrumentation)
  -> ( [Warning.Warning], Either [Error.Error] Compile.Artifacts )
compile docsFlag pkg importDict interfaces source reader =
  Result.run $ Compile.compile docsFlag pkg importDict interfaces source reader


consolidateSourceMaps
  :: Pkg.Name
  -> Map.Map M.Raw Compile.Artifacts
  -> SrcMap.Project
consolidateSourceMaps pkg artifacts =
  let
    addSourceMap home (Compile.Artifacts _ _ _ maybeSrcMap) projectSrcMap =
      case maybeSrcMap of
        Nothing ->
          projectSrcMap
        Just moduleSrcMap ->
          SrcMap.addModule (ModuleName.Canonical pkg home) moduleSrcMap projectSrcMap
  in
  Map.foldrWithKey addSourceMap SrcMap.emptyProject artifacts


-- ERRORS TO DOC


errorsToDoc :: FilePath -> Text.Text -> [Error.Error] -> D.Doc
errorsToDoc filePath source errors =
  let
    reports =
      concatMap (Error.toReports (Code.toSource source)) errors
  in
  D.vcat $ map (Report.toDoc filePath) reports



-- ERRORS TO JSON


errorsToJson :: M.Raw -> FilePath -> Text.Text -> [Error.Error] -> Encode.Value
errorsToJson moduleName filePath source errors =
  let
    reports =
      concatMap (Error.toReports (Code.toSource source)) errors
  in
  Encode.object
    [ ("path", Encode.text (Text.pack filePath))
    , ("name", Encode.name moduleName)
    , ("problems", Encode.array (map reportToJson reports))
    ]


reportToJson :: Report.Report -> Encode.Value
reportToJson (Report.Report title region _sgstns message) =
  Encode.object
    [ ("title", Encode.text (Text.pack title))
    , ("region", Region.encode region)
    , ("message", D.encode message)
    ]

{-# LANGUAGE OverloadedStrings #-}

module Reader.SourceMap
  ( Module(..)
  , Frame(..)
  , FrameId(..)
  , ExprId(..)
  , frameIdToText
  , exprIdToText
  , encodeModules
  )
  where


import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as B
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncode
import Data.Text (Text)

import qualified AST.Module.Name as ModuleName
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import qualified Reporting.Region as R

import qualified Json.Encode as Encode



-- SOURCE MAPS


data Module =
  Module
    { _frames :: Map.Map FrameId Frame
    , _source :: BS.ByteString
    }
    deriving (Show)

textShow :: (Show a) => a -> Text.Text
textShow = Text.pack . show

encodeModule :: Module -> Encode.Value
encodeModule (Module frames source) =
  Encode.object $
    [ ("source", Encode.text $ TextEncode.decodeUtf8 source)
    , ("frames", Encode.mapAsPairs ("id", encodeFrameId) ("frame", encodeFrame) frames)
    ]

encodeModules :: Map.Map ModuleName.Canonical Module -> Encode.Value
encodeModules =
  Encode.mapAsPairs
    ("module", encodeModuleName)
    ("source_map", encodeModule)

data Frame =
  Frame
    { _region :: R.Region
    , _exprRegions :: [(ExprId, R.Region)] -- May contain duplicate ids
    , _exprNames :: Map.Map ExprId (ModuleName.Canonical, N.Name) -- Some ids may not have entries
    }
    deriving (Show)

encodeFrame :: Frame -> Encode.Value
encodeFrame (Frame region exprRegions exprNames) =
  let
    regionsToMap :: [(ExprId, R.Region)] -> Map.Map ExprId [R.Region]
    regionsToMap regions =
      case regions of
          [] ->
              Map.empty
          (id, region) : rest ->
              let
                restMap :: Map.Map ExprId [R.Region]
                restMap = regionsToMap rest
              in
              case Map.lookup id restMap of
                  Just otherRegions ->
                    Map.insert id (region : otherRegions) restMap
                  Nothing ->
                    Map.insert id [region] restMap
    encodeQualifiedName (mod, name) =
      Encode.object $
        [ ( "module", encodeModuleName mod )
        , ( "name", Encode.text (N.toText name) )
        ]
  in
  Encode.object $
    [ ( "region"
      , R.encode region
      )
    , ( "expr_regions"
      , Encode.mapAsPairs
          ("id", encodeExprId)
          ("regions", Encode.array . map R.encode)
          (regionsToMap exprRegions)
      )
    , ( "expr_names"
      , Encode.mapAsPairs
          ("id", encodeExprId)
          ("qualified_name", encodeQualifiedName)
          exprNames
      )
    ]

-- IDENTIFIERS


data FrameId =
  FrameId
    { _module :: ModuleName.Canonical
    , _def :: N.Name
    , _frameIndex :: Int
    }
    deriving (Eq, Ord, Show)

encodeFrameId :: FrameId -> Encode.Value
encodeFrameId (FrameId mod def frameIdx) =
  Encode.object $
    [ ("module", encodeModuleName mod)
    , ("def", Encode.text $ N.toText def)
    , ("frame_index", Encode.int frameIdx)
    ]


encodeModuleName :: ModuleName.Canonical -> Encode.Value
encodeModuleName (ModuleName.Canonical (Pkg.Name author project) mod) =
  Encode.object $
    [ ( "package"
      ,  Encode.object $
          [ ("author", Encode.text author)
          , ("project", Encode.text project)
          ]
      )
    , ( "module"
      , Encode.text $ N.toText mod
      )
    ]

newtype ExprId = ExprId Int
  deriving (Eq, Ord, Show)

encodeExprId :: ExprId -> Encode.Value
encodeExprId (ExprId id) =
  Encode.int id


-- IDENTIFIERS AS TEXT


frameIdToText :: FrameId -> Text
frameIdToText frameId =
  let asJson = Encode.encode $ encodeFrameId frameId
      oneLine = Text.unwords . Text.lines
  in
  oneLine $ TextEncode.decodeUtf8 $ BSL.toStrict $ B.toLazyByteString asJson


exprIdToText :: ExprId -> Text
exprIdToText (ExprId index) =
  Text.pack $ show index

{-# LANGUAGE OverloadedStrings #-}

module Reader.SourceMap
  ( Project(..)
  , Module(..)
  , Frame(..)
  , FrameId(..)
  , ExprId(..)
  , QualifiedRegion
  , frameIdToText
  , encodeProject
  , emptyProject
  , addModule
  , regionIn
  , selfExprId
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



-- PROJECT SOURCE MAPS

data Project =
  Project
    { _allFrames :: Map.Map FrameId Frame
    , _sources :: Map.Map ModuleName.Canonical BS.ByteString
    }

emptyProject :: Project
emptyProject = Project Map.empty Map.empty

addModule :: ModuleName.Canonical -> Module -> Project -> Project
addModule modName (Module modFrames modSource) (Project frames sources) =
  Project
    { _allFrames = Map.union modFrames frames
    , _sources = Map.insert modName modSource sources
    }

encodeProject :: Project -> Encode.Value
encodeProject (Project frames sources) =
  Encode.object $
    [ ( "frames"
      , Encode.mapAsPairs
          ( "id", encodeFrameId )
          ( "frame", encodeFrame )
          frames
      )
    , ( "sources"
      , Encode.mapAsPairs
          ( "module", ModuleName.encode )
          ( "source", Encode.text . TextEncode.decodeUtf8 )
          sources
      )
    ]

-- MODULE SOURCE MAPS

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
    [ ( "source", Encode.text $ TextEncode.decodeUtf8 source )
    , ( "frames", Encode.mapAsPairs ("id", encodeFrameId) ("frame", encodeFrame) frames )
    ]

encodeModules :: Map.Map ModuleName.Canonical Module -> Encode.Value
encodeModules =
  Encode.mapAsPairs
    ( "module", ModuleName.encode )
    ( "source_map", encodeModule )

data Frame =
  Frame
    { _region :: QualifiedRegion
    , _exprRegions :: [(ExprId, QualifiedRegion)] -- May contain duplicate ids
    , _exprNames :: Map.Map ExprId (ModuleName.Canonical, N.Name) -- Some ids may not have entries
    }
    deriving (Show)

encodeFrame :: Frame -> Encode.Value
encodeFrame (Frame region exprRegions exprNames) =
  let
    regionsToMap :: [(ExprId, QualifiedRegion)] -> Map.Map ExprId [QualifiedRegion]
    regionsToMap regions =
      case regions of
          [] ->
              Map.empty
          (id, region) : rest ->
              let
                restMap :: Map.Map ExprId [QualifiedRegion]
                restMap = regionsToMap rest
              in
              case Map.lookup id restMap of
                  Just otherRegions ->
                    Map.insert id (region : otherRegions) restMap
                  Nothing ->
                    Map.insert id [region] restMap
    encodeQualifiedName (mod, name) =
      Encode.object $
        [ ( "module", ModuleName.encode mod )
        , ( "name", Encode.text (N.toText name) )
        ]
  in
  Encode.object $
    [ ( "region", encodeQualifiedRegion region )
    , ( "expr_regions"
      , Encode.mapAsPairs
          ( "id", encodeExprId )
          ( "regions", Encode.array . map encodeQualifiedRegion )
          (regionsToMap exprRegions)
      )
    , ( "expr_names"
      , Encode.mapAsPairs
          ("id", encodeExprId)
          ("qualified_name", encodeQualifiedName)
          exprNames
      )
    ]


-- QUALIFIED REGIONS

data QualifiedRegion =
  QualifiedRegion
    { _home :: ModuleName.Canonical
    , _start :: R.Position
    , _end :: R.Position
    }
    deriving (Eq, Ord, Show)

regionIn :: ModuleName.Canonical -> R.Region -> QualifiedRegion
regionIn mod region =
  QualifiedRegion mod (R._start region) (R._end region)

encodeQualifiedRegion :: QualifiedRegion -> Encode.Value
encodeQualifiedRegion (QualifiedRegion home start end) =
  Encode.object $
    [ ( "module", ModuleName.encode home )
    , ( "start", R.encodePosition start )
    , ( "end", R.encodePosition end )
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
    [ ( "module", ModuleName.encode mod )
    , ( "def", Encode.text $ N.toText def )
    , ( "frame_index", Encode.int frameIdx )
    ]


newtype ExprId = ExprId Int
  deriving (Eq, Ord, Show)

-- ExprId for the (return) value of a frame
selfExprId :: ExprId
selfExprId = ExprId (-1)

encodeExprId :: ExprId -> Encode.Value
encodeExprId (ExprId id) =
  Encode.int id


-- IDENTIFIERS AS TEXT


-- Note: any necessary escapment must occur in frameIdToText, whose result
-- will be put between single quotes in the output.
frameIdToText :: FrameId -> Text
frameIdToText frameId =
  let asJson = Encode.encode $ encodeFrameId frameId
      oneLine = Text.unwords . Text.lines
  in
  oneLine $ TextEncode.decodeUtf8 $ BSL.toStrict $ B.toLazyByteString asJson

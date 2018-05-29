{-# LANGUAGE OverloadedStrings #-}

module Reader.SourceMap
  ( Module(..)
  , Frame(..)
  , Expr(..)
  , FrameId(..)
  , ExprId(..)
  , frameIdToText
  , exprIdToText
  )
  where


import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)

import qualified AST.Module.Name as ModuleName
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import qualified Reporting.Region as R



-- SOURCE MAPS


data Module =
  Module
    { _frames :: Map.Map FrameId Frame
    }
    deriving (Show)


data Frame =
  Item
    { _region :: R.Region
    , _exprs :: Map.Map ExprId Expr
    }
    deriving (Show)


data Expr =
  Expr
    { _primaryRegion :: R.Region
    , _secondaryRegions :: [R.Region]
    , _fullName :: Maybe (ModuleName.Canonical, N.Name)
    }
    deriving (Show)



-- IDENTIFIERS


data FrameId =
  FrameId
    { _module :: ModuleName.Canonical
    , _def :: N.Name
    , _frameIndex :: Int
    }
    deriving (Eq, Ord, Show)


newtype ExprId = ExprId Int
  deriving (Eq, Ord, Show)



-- IDENTIFIERS AS TEXT


frameIdToText :: FrameId -> Text
frameIdToText (FrameId moduleName def index) =
  let
    (ModuleName.Canonical pkg module_) =
      moduleName
  in
    Pkg.toText pkg
    <> " " <> N.toText module_
    <> " " <> N.toText def
    <> " " <> Text.pack (show index)


exprIdToText :: ExprId -> Text
exprIdToText (ExprId index) =
  Text.pack $ show index

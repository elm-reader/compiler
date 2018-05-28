{-# LANGUAGE OverloadedStrings #-}

module Reader.SourceMap
  ( Module(..)
  , Item(..)
  , Expr(..)
  , ItemId(..)
  , ExprId(..)
  , itemIdToText
  , exprIdToText
  )
  where


import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)

import qualified AST.Module.Name as ModuleName
import qualified Elm.Name as N
import qualified Reporting.Region as R



-- SOURCE MAPS


data Module =
  Module
    { _items :: Map.Map ItemId Item
    }
    deriving (Show)


data Item =
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


data ItemId
  = Def N.Name
  | Lam N.Name Int
  deriving (Eq, Ord, Show)


data ExprId
  = Var N.Name
  | Anon Int
  deriving (Eq, Ord, Show)



-- IDENTIFIERS AS TEXT


itemIdToText :: ItemId -> Text
itemIdToText itemId =
  case itemId of
    Def name ->
      N.toText name

    Lam enclosingDef index ->
      N.toText enclosingDef <> " " <> Text.pack (show index)


exprIdToText :: ExprId -> Text
exprIdToText exprId =
  case exprId of
    Var name ->
      N.toText name

    Anon index ->
      Text.pack $ show index

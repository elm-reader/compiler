module Generate.JavaScript.Mode
  ( Mode(..)
  , Target(..)
  , debug
  , reader
  , dev
  , prod
  , isReader
  , isDebug
  , isServer
  )
  where


import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified Elm.Interface as I
import qualified Elm.Name as N
import qualified Generate.JavaScript.Name as Name
import qualified Reader.SourceMap as SrcMap



-- MODE


data Mode
  = Dev Target (Maybe I.Interfaces) (Maybe SrcMap.Project)
  | Prod Target ShortFieldNames


data Target = Client | Server


debug :: Target -> I.Interfaces -> Mode
debug target interfaces =
  Dev target (Just interfaces) Nothing


dev :: Target -> Mode
dev target =
  Dev target Nothing Nothing


reader :: Target -> I.Interfaces -> SrcMap.Project -> Mode
reader target ifaces srcMap =
  Dev target (Just ifaces) (Just srcMap)


prod :: Target -> Opt.Graph -> Mode
prod target (Opt.Graph _ _ fieldCounts) =
  Prod target (shortenFieldNames fieldCounts)



-- IS READER?


isReader :: Mode -> Bool
isReader mode =
  case mode of
    Dev _ _ maybeSrcMaps -> Maybe.isJust maybeSrcMaps
    Prod _ _ -> False



-- IS DEBUG?


isDebug :: Mode -> Bool
isDebug mode =
  case mode of
    Dev _ mi _ -> Maybe.isJust mi
    Prod _ _ -> False


-- IS SERVER?


isServer :: Mode -> Bool
isServer mode =
  case mode of
    Dev target _ _ -> isServerHelp target
    Prod target _ -> isServerHelp target


isServerHelp :: Target -> Bool
isServerHelp target =
  case target of
    Client -> False
    Server -> True



-- SHORTEN FIELD NAMES


type ShortFieldNames =
  Map.Map N.Name Name.Name


shortenFieldNames :: Map.Map N.Name Int -> ShortFieldNames
shortenFieldNames frequencies =
  Map.foldr addToShortNames Map.empty $
    Map.foldrWithKey addToBuckets Map.empty frequencies


addToBuckets :: N.Name -> Int -> Map.Map Int [N.Name] -> Map.Map Int [N.Name]
addToBuckets field frequency buckets =
  -- TODO try using an IntMap for buckets
  Map.insertWith (++) frequency [field] buckets


addToShortNames :: [N.Name] -> ShortFieldNames -> ShortFieldNames
addToShortNames fields shortNames =
  List.foldl' addField shortNames fields


addField :: ShortFieldNames -> N.Name -> ShortFieldNames
addField shortNames field =
  let rename = Name.fromInt (Map.size shortNames) in
  Map.insert field rename shortNames

module Reader.Instrument
  ( instrument
  )
  where


import Text.Show.Pretty (ppShow)
import Debug.Trace (trace)

import qualified Data.Map as Map

import qualified AST.Canonical as Can
import qualified Reader.SourceMap as SrcMap



instrument :: Can.Module -> (Can.Module, SrcMap.Module)
instrument module_ =
  trace ("elm-reader debug -- received canonical AST:\n" ++ ppShow module_) $
    instrumentModule module_


instrumentModule :: Can.Module -> (Can.Module, SrcMap.Module)
instrumentModule module_ =
  (module_, SrcMap.Module Map.empty)

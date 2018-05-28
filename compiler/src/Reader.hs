module Reader (annotate) where

import Text.Show.Pretty (ppShow)
import Debug.Trace (trace)

import qualified AST.Canonical as Can

annotate :: Can.Module -> Can.Module
annotate module_ =
  trace ("elm-reader debug -- received canonical AST:\n" ++ ppShow module_) module_

module Reader (annotate) where

import qualified AST.Canonical as Can

annotate :: Can.Module -> Can.Module
annotate = id

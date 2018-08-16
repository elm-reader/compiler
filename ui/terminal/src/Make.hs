module Make
  ( Flags(..)
  , run
  , ReportType(..)
  , reportType
  , docsFile
  )
  where


import qualified System.FilePath as FP

import qualified Elm.Compiler as Compiler
import qualified Elm.Project as Project
import qualified Generate.Output as Output
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Make as E
import qualified Reporting.Task as Task
import qualified Reporting.Progress as Progress
import qualified Reporting.Progress.Json as Json
import qualified Reporting.Progress.Terminal as Terminal
import Terminal.Args (Parser(..))



-- RUN


data Flags =
  Flags
    { _debug :: Bool
    , _optimize :: Bool
    , _output :: Maybe Output.Output
    , _report :: Maybe ReportType
    , _read :: Bool
    , _docs :: Maybe FilePath
    }


run :: [FilePath] -> Flags -> IO ()
run paths (Flags debug optimize output report reader docs) =
  do  reporter <- toReporter report
      Task.run reporter $
        do  mode <- toMode debug reader optimize
            summary <- Project.getRoot
            Project.compile mode Output.Client output docs summary paths


toMode :: Bool -> Bool -> Bool -> Task.Task Output.Mode
toMode debug reader optimize =
  case (debug, reader, optimize) of
    (True , _    , True ) -> Task.throw $ Exit.Make E.CannotOptimizeAndDebug
    -- TODO: can we optimize and read?
    (False, _    , True ) -> return Output.Prod
    (_    , True , False) -> return Output.Reader
    (True , False, False) -> return Output.Debug
    (False, False, False) -> return Output.Dev


toReporter :: Maybe ReportType -> IO Progress.Reporter
toReporter report =
  case report of
    Nothing -> Terminal.create
    Just Json -> return Json.reporter



-- REPORT


data ReportType
  = Json


reportType :: Parser ReportType
reportType =
  Parser
    { _singular = "report type"
    , _plural = "report types"
    , _parser = \string -> if string == "json" then Just Json else Nothing
    , _suggest = \_ -> return ["json"]
    , _examples = \_ -> return ["json"]
    }



-- DOCS


docsFile :: Parser FilePath
docsFile =
  Parser
    { _singular = "json file"
    , _plural = "json files"
    , _parser = \string -> if FP.takeExtension string == ".json" then Just string else Nothing
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["docs.json","documentation.json"]
    }



-- READER

toReaderFlag :: Bool -> Compiler.ReaderFlag
toReaderFlag True = Compiler.YesReader
toReaderFlag False = Compiler.NoReader

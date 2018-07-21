port module Reader.Tracing
    exposing
        ( requestTrace
        , traces
        )

import Reader.ProgramConfig as PC


port traces : (String -> msg) -> Sub msg


port requestTrace : () -> Cmd msg

module Reader.Msg exposing (Msg(..))

import Reader.TraceData as TraceData exposing (TraceData)


type Msg
    = SelectTopLevelFrame TraceData.InstrumentedFrameData
    | HoverExpr TraceData.ExprWithContext
    | OpenChildFrame TraceData.FrameId

module Reader.SelectedFrameTree
    exposing
        ( SelectedFrameTree
        , frameIdOf
        , fromTrace
        , getOpenFrames
        , openFrame
        )

import Reader.SourceMap as SourceMap exposing (SourceMap)
import Reader.TraceData as TraceData exposing (TraceData)


type SelectedFrameTree
    = SelectedFrameTree
        -- the selected frame:
        TraceData.Frame
        -- the open child frame, if any
        (Maybe TraceData.FrameId)
        -- all the child frames (including the open one):
        (List SelectedFrameTree)


fromTrace : TraceData.Frame -> SelectedFrameTree
fromTrace frame =
    SelectedFrameTree frame Nothing (List.map fromTrace (TraceData.childFrames frame))


frameIdOf : SelectedFrameTree -> TraceData.FrameId
frameIdOf (SelectedFrameTree frame _ _) =
    TraceData.frameIdOf frame


openFrame : TraceData.FrameId -> SelectedFrameTree -> SelectedFrameTree
openFrame childFrameId =
    let
        isTarget (SelectedFrameTree otherFrame _ _) =
            TraceData.frameIdsEqual
                childFrameId
                (TraceData.frameIdOf otherFrame)

        isAncestor =
            frameIdOf >> TraceData.isAncestorOf childFrameId

        openFrameIn ((SelectedFrameTree otherFrame maybeOpenChildId children) as selTree) =
            if isTarget selTree then
                selTree
            else
                let
                    combine sf ( newChild, restChildren ) =
                        if isTarget sf || isAncestor sf then
                            ( Just (frameIdOf sf)
                            , openFrameIn sf :: restChildren
                            )
                        else
                            ( newChild, sf :: restChildren )

                    ( maybeNewOpenChild, newChildren ) =
                        children
                            |> List.foldr combine ( Nothing, [] )
                in
                SelectedFrameTree otherFrame maybeNewOpenChild newChildren
    in
    openFrameIn


getOpenFrames :
    SelectedFrameTree
    -> List TraceData.Frame -- TODO: include more data than TraceData.Frame
getOpenFrames (SelectedFrameTree thisFrame maybeOpenChildId children) =
    case maybeOpenChildId of
        Nothing ->
            [ thisFrame ]

        Just childId ->
            case List.filter (frameIdOf >> TraceData.frameIdsEqual childId) children of
                [ openChild ] ->
                    thisFrame :: getOpenFrames openChild

                [] ->
                    Debug.log
                        "ERROR: did not find open child ID in child frames"
                        [ thisFrame ]

                (anOpenChild :: _ :: _) as duplicates ->
                    let
                        _ =
                            Debug.log
                                "ERROR: found duplicate child frames with same ID"
                                (List.map frameIdOf duplicates)
                    in
                    thisFrame
                        :: getOpenFrames anOpenChild

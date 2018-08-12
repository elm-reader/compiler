module Reader.FrameUI
    exposing
        ( view
        )

{-| Reader.FrameUI manages the UI of a single stack frame.
TODO: When we add term pinning, this should be a good place to manage that
state within a SelectedFrameTree.
-}

import Reader.FrameUI.View


view =
    Reader.FrameUI.View.viewTrace

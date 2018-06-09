module Reader exposing (read)

{-|

    Reader.

    @docs read

-}

import Elm.Kernel.Reader


{-|

    Reader.read

-}
read : String -> Int -> a -> a
read =
    Elm.Kernel.Reader.read

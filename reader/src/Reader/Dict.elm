module Reader.Dict
    exposing
        ( Dict
        , empty
        , fromList
        )


type Dict key value
    = Dict (List ( key, value ))


fromList =
    Dict


empty =
    fromList []

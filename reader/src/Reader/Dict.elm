module Reader.Dict
    exposing
        ( Dict
        , empty
        , fromList
        , insert
        , keys
        , lookup
        , values
        )


type Dict key value
    = Dict (List ( key, value ))


fromList : List ( k, v ) -> Dict k v
fromList =
    Dict


empty : Dict k v
empty =
    fromList []


insert : k -> v -> Dict k v -> Dict k v
insert k v (Dict pairs) =
    Dict <| ( k, v ) :: pairs


lookup : k -> Dict k v -> Maybe v
lookup k (Dict pairs) =
    lookupPairs k pairs


lookupPairs : k -> List ( k, v ) -> Maybe v
lookupPairs targetKey pairs =
    case pairs of
        [] ->
            Nothing

        ( k, v ) :: rest ->
            if k == targetKey then
                Just v
            else
                lookupPairs targetKey rest


keys : Dict k v -> List k
keys (Dict pairs) =
    List.map (\( key, _ ) -> key) pairs


values : Dict k v -> List v
values (Dict pairs) =
    List.map (\( _, value ) -> value) pairs

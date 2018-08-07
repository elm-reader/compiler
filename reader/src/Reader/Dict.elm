module Reader.Dict
    exposing
        ( Dict
        , decode
        , empty
        , fromList
        , insert
        , keyValuePairs
        , keys
        , lookup
        , values
        )

import Json.Decode as JD


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


keyValuePairs : Dict k v -> List ( k, v )
keyValuePairs (Dict pairs) =
    pairs


decode : ( String, JD.Decoder key ) -> ( String, JD.Decoder value ) -> JD.Decoder (Dict key value)
decode ( keyName, decodeKey ) ( valName, decodeVal ) =
    let
        decodeEntry =
            JD.map2 Tuple.pair
                (JD.field keyName decodeKey)
                (JD.field valName decodeVal)
    in
    JD.map fromList (JD.list decodeEntry)

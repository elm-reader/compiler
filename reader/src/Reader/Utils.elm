module Reader.Utils exposing (reverseSortWith)


reverseSortWith : (a -> a -> Order) -> List a -> List a
reverseSortWith cmp =
    let
        invertedCmp a b =
            case cmp a b of
                EQ ->
                    EQ

                LT ->
                    GT

                GT ->
                    LT
    in
    List.sortWith cmp

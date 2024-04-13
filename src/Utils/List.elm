module Utils.List exposing (firstWhere, firstWhereIndexed)


{-
Return the first element in `list` that satisfies `pred`, if it exists.
-}
firstWhere : (a -> Bool) -> List a -> Maybe a
firstWhere pred list =
    firstWhereIndexed pred list
        |> Maybe.map Tuple.first


firstWhereIndexed : (a -> Bool) -> List a -> Maybe (a, Int)
firstWhereIndexed pred list =
    let
        acc pred_ list_ i =
            case list_ of
                [] -> Nothing
                first::rest ->
                    if pred_ first then
                        Just (first, i)
                    else
                        acc pred_ rest (i + 1)
    in
    acc pred list 0

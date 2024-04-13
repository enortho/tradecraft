module Utils.Array exposing (map3, any, all)

import Array exposing (Array)

map3 : (a -> b -> c -> value) -> Array a -> Array b -> Array c -> Array value
map3 fn a1 a2 a3 =
    let
        acc fn_ a1_ a2_ a3_ i acc_ =
            case
                Maybe.map3 fn_
                    (Array.get i a1)
                    (Array.get i a2)
                    (Array.get i a3)
            of
                Nothing -> acc_
                Just val ->
                    acc fn_ a1_ a2_ a3_ (i + 1) (Array.push val acc_)
    in
    acc fn a1 a2 a3 0 Array.empty


all : (a -> Bool) -> Array a -> Bool
all fn array =
    let
        acc fn_ array_ i =
            case Array.get i array_ of
                Nothing ->
                    True

                Just val ->
                    if fn val then
                        acc fn_ array_ (i + 1)

                    else
                        False
    in
    acc fn array 0


any : (a -> Bool) -> Array a -> Bool
any fn array =
    let
        acc fn_ array_ i =
            case Array.get i array_ of
                Nothing ->
                    False

                Just val ->
                    if fn_ val then
                        True

                    else
                        acc fn_ array_ (i + 1)
    in
    acc fn array 0

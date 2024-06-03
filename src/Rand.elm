module Rand exposing (..)

import Event exposing (Deal)
import Random as R exposing (Generator)
import Resource exposing (Resource)


generateDeal : Resource -> Resource -> List Resource -> Generator Deal
generateDeal first_ second_ rest =
    R.uniform ( first_, second_ ) [ ( second_, first_ ) ]
        |> R.andThen
            (\( first, second ) ->
                R.map2 Tuple.pair
                    (R.uniform first rest)
                    (R.int 1 200)
                    |> R.andThen
                        (\sell ->
                            R.map2 Tuple.pair
                                (R.uniform second rest)
                                (R.int 1 200)
                                |> R.map
                                    (\buy ->
                                        { sell = sell
                                        , buy = buy
                                        , events = []
                                        }
                                    )
                        )
            )



{- R.map2 Tuple.pair
   (R.uniform first (second::rest))
   (R.int 1 200)
   |> R.andThen
       (\buy ->
           R.map2 Tuple.pair
               (R.uniform first rest)
               (R.int 1 200)
               |> R.map
                   (\sell ->
                       { sell = sell
                       , buy = buy
                       , events = []
                       }
                   )
       )
-}

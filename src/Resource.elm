module Resource exposing (Resource, coin, wood, bricks, valueMap, resources, prev, next, between, best, generator)

import Dict exposing (Dict)
import Random
import List exposing (append)

type alias Resource =
  { name : String
  , value : Int
  , image : String
  , backgroundColor : String
  }


coin : Resource
coin =
  { name = "coin"
  , value = 1
  , image = "/coin.png"
  , backgroundColor = "#ffff88"
  }

wood : Resource
wood =
  { name = "wood"
  , value = 2
  , image = "/wood.png"
  , backgroundColor = "#bb6800"
  }

bricks : Resource
bricks =
  { name = "bricks"
  , value = 5
  , image = "/bricks.png"
  , backgroundColor = ""
  }

valueMap : Dict String Resource
valueMap =
    Dict.fromList
        [ ( "coin", coin )
        , ( "wood", wood )
        , ( "brick", bricks )
        ]
resources : List Resource
resources =
    [ coin
    , wood
    , bricks
    ]

generator : Random.Generator Resource
generator =
    Random.uniform
        coin
        (List.tail resources |> Maybe.withDefault [])



listFirstWhere : (a -> Bool) -> List a -> Maybe a
listFirstWhere fn list =
    case list of
        [] -> Nothing
        first::rest ->
            if fn first then
                Just first
            else
                listFirstWhere fn rest

listLastWhere : (a -> Bool) -> List a -> Maybe a
listLastWhere fn list =
    listFirstWhere fn (List.reverse list)

prev : Resource -> Maybe Resource
prev resource =
    listLastWhere (\res -> res.value < resource.value) resources

next : Resource -> Maybe Resource
next resource =
    listFirstWhere (\res -> res.value > resource.value) resources

applyN : Int -> (a -> a) -> a -> a
applyN n fn val =
    if n <= 0 then
        val
    else
        applyN (n - 1) fn (fn val)

nextN : Int -> Resource -> Maybe Resource
nextN n resource =
    if n <= 0 then
        Just resource
    else
        next resource
            |> Maybe.andThen (nextN (n - 1))
prevN : Int -> Resource -> Maybe Resource
prevN n resource =
    if n <= 0 then
        Just resource
    else
        prev resource
            |> Maybe.andThen (prevN (n - 1))

between : Resource -> Resource -> List Resource
between low high =
    List.filter
        (\res -> low.value < res.value && res.value < high.value)
        resources

best : Resource -> List Resource -> Resource
best first rest =
    List.foldl
        (\res acc -> if res.value > acc.value then res else acc)
        first
        rest

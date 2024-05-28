module Resource exposing (Resource, coin, wood, bricks, valueMap, resources)

import Dict exposing (Dict)

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

module Resource exposing (..)

import Dict exposing (Dict)
import Random
import List exposing (append)
import Array exposing (Array)

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


copper : Resource
copper =
    { name = "copper"
    , value = 10
    , image = "/copper.png"
    , backgroundColor = ""
    }


iron : Resource
iron =
    { name = "iron"
    , value = 100
    , image = "/iron.png"
    , backgroundColor = ""
    }


silver : Resource
silver =
    { name = "silver"
    , value = 250
    , image = "/silver.png"
    , backgroundColor = ""
    }


gold : Resource
gold =
    { name = "gold"
    , value = 530
    , image = "/gold.png"
    , backgroundColor = ""
    }


plat : Resource
plat =
    { name = "plat"
    , value = 1340
    , image = "/plat.png"
    , backgroundColor = ""
    }


emerald : Resource
emerald =
    { name = "emerald"
    , value = 5800
    , image = "/emerald.png"
    , backgroundColor = ""
    }


ruby : Resource
ruby =
    { name = "ruby"
    , value = 24000
    , image = "/ruby.png"
    , backgroundColor = ""
    }


opal : Resource
opal =
    { name = "opal"
    , value = 82000
    , image = "/opal.png"
    , backgroundColor = ""
    }


amethyst : Resource
amethyst =
    { name = "amethyst"
    , value = 450000
    , image = "/amethyst.png"
    , backgroundColor = ""
    }


citrine : Resource
citrine =
    { name = "citrine"
    , value = 1000000
    , image = "/citrine.png"
    , backgroundColor = ""
    }


carbon : Resource
carbon =
    { name = "carbon"
    , value = 4500000
    , image = "/carbon.png"
    , backgroundColor = ""
    }


diamond : Resource
diamond =
    { name = "diamond"
    , value = 14000000
    , image = "/diamond.png"
    , backgroundColor = ""
    }


purplemetal : Resource
purplemetal =
    { name = "purplemetal"
    , value = 53000000
    , image = "/purplemetal.png"
    , backgroundColor = ""
    }


bloodgem : Resource
bloodgem =
    { name = "bloodgem"
    , value = 160000000
    , image = "/bloodgem.png"
    , backgroundColor = ""
    }


stardust : Resource
stardust =
    { name = "stardust"
    , value = 999999999
    , image = "/stardust.png"
    , backgroundColor = ""
    }


firstResource : Resource
firstResource = coin


restResources : List Resource
restResources =
    [ wood
    , bricks
    ]

resourceArray : Array Resource
resourceArray =
    Array.fromList
        [ coin
        , wood
        , bricks
        ]


numResources : Int
numResources = Array.length resourceArray

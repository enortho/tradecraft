module Resource exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import List exposing (append)
import Random


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
    , image = "tradecraft/coin.png"
    , backgroundColor = "#ffff88"
    }


wood : Resource
wood =
    { name = "wood"
    , value = 2
    , image = "tradecraft/wood.png"
    , backgroundColor = "#bb6800"
    }


bricks : Resource
bricks =
    { name = "bricks"
    , value = 5
    , image = "tradecraft/bricks.png"
    , backgroundColor = "#ff9900"
    }


copper : Resource
copper =
    { name = "copper"
    , value = 10
    , image = "tradecraft/copper.png"
    , backgroundColor = "#dd3333"
    }


iron : Resource
iron =
    { name = "iron"
    , value = 100
    , image = "tradecraft/iron.png"
    , backgroundColor = ""
    }


silver : Resource
silver =
    { name = "silver"
    , value = 250
    , image = "tradecraft/silver.png"
    , backgroundColor = ""
    }


gold : Resource
gold =
    { name = "gold"
    , value = 530
    , image = "tradecraft/gold.png"
    , backgroundColor = ""
    }


plat : Resource
plat =
    { name = "plat"
    , value = 1340
    , image = "tradecraft/plat.png"
    , backgroundColor = ""
    }


emerald : Resource
emerald =
    { name = "emerald"
    , value = 5800
    , image = "tradecraft/emerald.png"
    , backgroundColor = ""
    }


ruby : Resource
ruby =
    { name = "ruby"
    , value = 24000
    , image = "tradecraft/ruby.png"
    , backgroundColor = ""
    }


opal : Resource
opal =
    { name = "opal"
    , value = 82000
    , image = "tradecraft/opal.png"
    , backgroundColor = ""
    }


amethyst : Resource
amethyst =
    { name = "amethyst"
    , value = 450000
    , image = "tradecraft/amethyst.png"
    , backgroundColor = ""
    }


citrine : Resource
citrine =
    { name = "citrine"
    , value = 1000000
    , image = "tradecraft/citrine.png"
    , backgroundColor = ""
    }


carbon : Resource
carbon =
    { name = "carbon"
    , value = 4500000
    , image = "tradecraft/carbon.png"
    , backgroundColor = ""
    }


diamond : Resource
diamond =
    { name = "diamond"
    , value = 14000000
    , image = "tradecraft/diamond.png"
    , backgroundColor = ""
    }


purplemetal : Resource
purplemetal =
    { name = "purplemetal"
    , value = 53000000
    , image = "tradecraft/purplemetal.png"
    , backgroundColor = ""
    }


bloodgem : Resource
bloodgem =
    { name = "bloodgem"
    , value = 160000000
    , image = "tradecraft/bloodgem.png"
    , backgroundColor = ""
    }


stardust : Resource
stardust =
    { name = "stardust"
    , value = 999999999
    , image = "tradecraft/stardust.png"
    , backgroundColor = ""
    }


firstResource : Resource
firstResource =
    coin


restResources : List Resource
restResources =
    [ wood
    , bricks
    , copper
    , iron
    , silver
    , gold
    , plat
    , emerald
    , ruby
    , opal
    , amethyst
    , citrine
    , carbon
    , diamond
    , purplemetal
    , bloodgem
    , stardust
    ]


resourceArray : Array Resource
resourceArray =
    Array.fromList <| firstResource :: restResources


numResources : Int
numResources =
    Array.length resourceArray

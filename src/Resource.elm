module Resource exposing (Resource, coin, wood, resources, bricks)

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

resources : List Resource
resources = [coin, wood, bricks]

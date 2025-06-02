module GameThings exposing (..)

import Resource as Res
import Event exposing (ResourceTrigger)

initTrigger : ResourceTrigger
initTrigger =
  { resourcesNeeded = [ ( Res.coin, 16 )]
  , events =
      [ Event.AddDeal 0
          { sell = ( Res.coin, 50 )
          , buy = ( Res.wood, 35)
          , events =
              [ Event.SetClickValueForResource "wood" 1
              , Event.AddResourceTrigger
                  { resourcesNeeded = [ ( Res.coin, 16 )]
                  , events =
                      [ Event.AddQuest Event.First
                          { title = "Work gloves"
                          , description = "This is your first upgrade. Lets you click for wood"
                          , cost = [ ( Res.coin, 30 ), ( Res.wood, 10 )]
                          , events = 
                              [ Event.AddDeal 0
                                  { sell = (Res.wood, 30)
                                  , buy = (Res.bricks, 20)
                                  , events = []
                                  }
                              , Event.SetClickValueForResource Res.wood.name 1
                              , Event.AddQuest Event.First
                                  { title = "More deals!"
                                  , description = "Start getting more trading deals. Each resource has a certain value, try to gain value by trading."
                                  , cost = [(Res.coin, 30), (Res.wood, 20), (Res.bricks, 10)]
                                  , events = 
                                      [ Event.StartGeneratingDeals
                                      , Event.AddQuest Event.First
                                          { title = "Money!"
                                          , description = "Each click on the coin gives you two coins."
                                          , cost = [(Res.coin, 150), (Res.wood, 10), (Res.bricks, 10)]
                                          , events = 
                                              [ Event.SetClickValueForResource Res.coin.name 2
                                              , Event.AddQuest Event.First
                                                  { title = "DIY brick set"
                                                  , description = "Lets you get bricks manually."
                                                  , cost = [(Res.coin, 60), (Res.bricks, 20)]
                                                  , events =
                                                      [ Event.SetClickValueForResource Res.bricks.name 1
                                                      , Event.AddQuest Event.First
                                                          { title = "Iron axe"
                                                          , description = "Mine wood twice as fast!"
                                                          , cost = [(Res.wood, 20), (Res.iron, 2)]
                                                          , events =
                                                              [ Event.AddQuest Event.First
                                                                  { title = "Multitasking"
                                                                  , description = "Open another upgrade path and always have two upgrades to choose from!"
                                                                  , cost = [(Res.bricks, 10), (Res.copper, 8), (Res.iron, 5)]
                                                                  , events = 
                                                                      [ Event.AddQuest Event.First
                                                                          { title = "Spaaace"
                                                                          , description = "Gain 4 additional tiles for deals."
                                                                          , cost = [(Res.wood, 25), (Res.bricks, 10), (Res.copper, 20)]
                                                                          , events =
                                                                              [ Event.AddDealCapacity 4
                                                                              , Event.AddQuest Event.First
                                                                                    { title = "Mine cart"
                                                                                    , description = "Mine copper. Clang."
                                                                                    , cost = [(Res.coin, 300), (Res.wood, 200), (Res.iron, 10)]
                                                                                    , events = 
                                                                                        [ Event.SetClickValueForResource Res.copper.name 1
                                                                                        , Event.AddQuest Event.First
                                                                                            { title = "Utility tech tree"
                                                                                            , description = "Open third upgrade path focusing on unique upgrades affecting trade offers."
                                                                                            , cost = [(Res.silver, 5), (Res.gold, 1)]
                                                                                            , events =
                                                                                                [ Event.AddQuest Event.First
                                                                                                    { title = "Making money 101"
                                                                                                    , description = "Gain 10 coins with each click!"
                                                                                                    , cost = [(Res.coin, 500), (Res.silver, 5), (Res.gold, 2)]
                                                                                                    , events =
                                                                                                        [ Event.SetClickValueForResource Res.coin.name 10
                                                                                                        , Event.AddQuest Event.First
                                                                                                            { title = "Metal Detector"
                                                                                                            , description = "Collect iron faster than ever before! Well, that is not that hard tbh..."
                                                                                                            , cost = [(Res.plat, 5), (Res.emerald, 5), (Res.ruby, 2)]
                                                                                                            , events = 
                                                                                                                []
                                                                                                            }
                                                                                                        ]
                                                                                                    }
                                                                                                , Event.AddQuest Event.Third
                                                                                                    { title = "Wide connections"
                                                                                                    , description = "Add 4 additional tiles for trades."
                                                                                                    , cost = [(Res.coin, 1000), (Res.silver, 30), (Res.gold, 10)]
                                                                                                    , events = 
                                                                                                        [
                                                                                
                                                                                                        ]
                                                                                                    }
                                                                                                ]
                                                                                            }
                                                                                        ]
                                                                                    }
                                                                              , Event.AddQuest Event.Second
                                                                                  { title = "Invest"
                                                                                  , description = "Gain money automatically. Passive income, the dream."
                                                                                  , cost = [(Res.coin, 600)]
                                                                                  , events = 
                                                                                      [ Event.SetAutoGeneration Res.coin 1000
                                                                                      , Event.AddQuest Event.Second
                                                                                          { title = "Treants"
                                                                                          , description = "Trees go to your inventory on their own."
                                                                                          , cost = [(Res.wood, 800), (Res.silver, 10)]
                                                                                          , events = 
                                                                                              [ Event.SetAutoGeneration Res.wood 1000
                                                                                              , Event.AddQuest Event.Second
                                                                                                    { title = "Tiny golems"
                                                                                                    , description = "Golems make bricks out of themselves. It is brutal but you get bricks from it."
                                                                                                    , cost = [(Res.coin, 5000), (Res.bricks, 1000), (Res.gold, 4)]
                                                                                                    , events = 
                                                                                                        [ Event.SetAutoGeneration Res.bricks 1000
                                                                                                        , Event.AddQuest Event.Second
                                                                                                            { title = "Lawyer"
                                                                                                            , description = "Double the amount of coins you gain automatically!"
                                                                                                            , cost = [(Res.coin, 3000)]
                                                                                                            , events = 
                                                                                                                [ Event.SetAutoGeneration Res.coin 1000
                                                                                                                , Event.AddQuest Event.Second
                                                                                                                    { title = "Motherload Machine"
                                                                                                                    , description = "Gain copper over time! And a bit of iron on top of that!"
                                                                                                                    , cost = [(Res.silver, 50), (Res.gold, 30), (Res.plat, 10)]
                                                                                                                    , events = []
                                                                                                                    }
                                                                                                                ]
                                                                                                            }
                                                                                                        ]
                                                                                                    }
                                                                                              ]
                                                                                          }
                                                                                      ]
                                                                                  }
                                                                              ]
                                                                          }
                                                                      ]
                                                                  }
                                                              ]
                                                          }
                                                      ]
                                                  }
                                              ]
                                          }
                                      ]
                                  }
                              ]
                          }
                      ]
                  }
              ]
          }
      ]
  }

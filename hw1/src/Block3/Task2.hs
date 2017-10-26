module Block3.Task2 where

data Stats = Stats { health
                   , attackPower
                   , armor :: Int
                   }

class (Show a) => Creature a where
   getStats :: a -> Stats

   setStats :: a -> Stats -> a

   isAlive :: a -> Bool
   isAlive = (> 0) . health . getStats


data Knight = Knight { knightName  :: String
                     , knightStats :: Stats
                     }

data Monster = Monster { monsterType  :: String
                       , monsterStats :: Stats
                       }

instance Show Stats where
    show stats = "health: " ++ show (health stats) ++
                 ", attack power: " ++ show (attackPower stats) ++
                 ", armor: " ++ show (armor stats)

instance Show Knight where
    show (Knight name stats)  = "{~ " ++ name ++ " -- " ++ show stats ++ " ~}"

instance Show Monster where
    show (Monster mosterType stats) = "<~ " ++ mosterType ++ " -- " ++ show stats ++ " ~>"

instance Creature Knight where
    getStats = knightStats

    setStats knight stats = knight { knightStats = stats }

instance Creature Monster where
    getStats = monsterStats

    setStats monster stats = monster { monsterStats = stats}

strike :: (Creature a, Creature b) => a -> b -> b
strike a b = setStats b damagedStats
  where
    aStats = getStats a
    bStats = getStats b
    damagedStats = Stats { health = health bStats - attackPower aStats *
                             (if armor bStats > 100
                              then 1
                              else round $ ((100 :: Double) - fromIntegral (armor bStats)) / 100)
                         , attackPower = attackPower bStats
                         , armor = if armor bStats > 0
                                   then armor bStats - attackPower aStats `div` 10
                                   else 0
                         }

allAlive :: (Creature a, Creature b) => (a, b) -> Bool
allAlive (a, b) = isAlive a && isAlive b

showWinner :: (Creature a, Creature b) => (a, b) -> String
showWinner (a, b) | allAlive (a, b)  = error "Both opponents are still alive"
                  | not (isAlive a) && not (isAlive b) = error "Both opponents are dead"
                  | isAlive a = "Winner: " ++ show a
                  | otherwise = "Winner: " ++ show b

fight :: (Creature a, Creature b) => a -> b -> String
fight a b | allAlive (a, b) = fight (strike a b) a
          | otherwise = showWinner (a, b)

arthas :: Knight
arthas = Knight { knightName = "Arthas"
                , knightStats = Stats { health = 1100
                                , attackPower = 42
                                , armor = 30
                                }
                }

cobold :: Monster
cobold = Monster { monsterType = "Cobold"
                 , monsterStats = Stats { health = 300
                                        , attackPower = 10
                                        , armor = 5
                                        }
                 }

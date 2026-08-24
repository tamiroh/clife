module Algorithm
  ( Algorithm (..),
    rulesFor,
  )
where

import LifeLike (Rules (..))

data Algorithm
  = Conway
  | HighLife

rulesFor :: Algorithm -> Rules
rulesFor algorithm =
  case algorithm of
    Conway -> conwayRules
    HighLife -> highLifeRules

----------------------------------------------------------------------
-- Conway
----------------------------------------------------------------------

conwayRules :: Rules
conwayRules =
  Rules
    { surviveWhenNeighborsAre = [2, 3],
      birthWhenNeighborsAre = [3]
    }

----------------------------------------------------------------------
-- HighLife
----------------------------------------------------------------------

highLifeRules :: Rules
highLifeRules =
  Rules
    { surviveWhenNeighborsAre = [2, 3],
      birthWhenNeighborsAre = [3, 6]
    }

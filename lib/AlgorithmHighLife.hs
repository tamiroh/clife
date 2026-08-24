module AlgorithmHighLife
  ( rules,
  )
where

import LifeLike (Rules (..))

rules :: Rules
rules =
  Rules
    { surviveWhenNeighborsAre = [2, 3],
      birthWhenNeighborsAre = [3, 6]
    }

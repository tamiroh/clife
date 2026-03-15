module AlgorithmHighLife
  ( algorithmImpl,
  )
where

import AlgorithmImpl (AlgorithmImpl (..))
import LifeLike (Rules (..), nextBoardWithRules)

rules :: Rules
rules =
  Rules
    { surviveWhenNeighborsAre = [2, 3],
      birthWhenNeighborsAre = [3, 6]
    }

algorithmImpl :: AlgorithmImpl
algorithmImpl = AlgorithmImpl {nextBoard = nextBoardWithRules rules}

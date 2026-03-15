module AlgorithmConway
  ( algorithmImpl,
  )
where

import AlgorithmImpl (AlgorithmImpl (..))
import LifeLike (Rules (..), nextBoardWithRules)

rules :: Rules
rules =
  Rules
    { surviveWhenNeighborsAre = [2, 3],
      birthWhenNeighborsAre = [3]
    }

algorithmImpl :: AlgorithmImpl
algorithmImpl = AlgorithmImpl {nextBoard = nextBoardWithRules rules}

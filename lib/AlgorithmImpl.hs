module AlgorithmImpl
  ( AlgorithmImpl (..),
  )
where

import Board (Board)

newtype AlgorithmImpl = AlgorithmImpl
  { nextBoard :: Board -> Board
  }

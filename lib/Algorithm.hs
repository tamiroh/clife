module Algorithm
  ( Algorithm (..),
    algorithmImplFor,
  )
where

import qualified AlgorithmConway
import qualified AlgorithmHighLife
import AlgorithmImpl (AlgorithmImpl)

data Algorithm
  = Conway
  | HighLife

algorithmImplFor :: Algorithm -> AlgorithmImpl
algorithmImplFor algorithm =
  case algorithm of
    Conway -> AlgorithmConway.algorithmImpl
    HighLife -> AlgorithmHighLife.algorithmImpl

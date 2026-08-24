module Algorithm
  ( Algorithm (..),
    rulesFor,
  )
where

import qualified AlgorithmConway
import qualified AlgorithmHighLife
import LifeLike (Rules)

data Algorithm
  = Conway
  | HighLife

rulesFor :: Algorithm -> Rules
rulesFor algorithm =
  case algorithm of
    Conway -> AlgorithmConway.rules
    HighLife -> AlgorithmHighLife.rules

module LifeLike
  ( Rules (..),
    nextBoardWithRules,
  )
where

import Board (Board, Cell, isAlive, liveCells, setLiveCells)
import qualified Data.Set as Set

data Rules = Rules
  { surviveWhenNeighborsAre :: [Int],
    birthWhenNeighborsAre :: [Int]
  }

neighbors :: Cell -> [Cell]
neighbors (x, y) =
  [ (x - 1, y - 1),
    (x, y - 1),
    (x + 1, y - 1),
    (x - 1, y),
    (x + 1, y),
    (x - 1, y + 1),
    (x, y + 1),
    (x + 1, y + 1)
  ]

liveNeighbors :: Board -> Cell -> Int
liveNeighbors board cell =
  length [neighbor | neighbor <- neighbors cell, isAlive board neighbor]

survivors :: Rules -> Board -> Set.Set Cell
survivors rules board =
  Set.filter survives (liveCells board)
  where
    survives cell =
      let n = liveNeighbors board cell
       in n `elem` surviveWhenNeighborsAre rules

births :: Rules -> Board -> Set.Set Cell
births rules board =
  Set.filter isBirthCell (birthCandidates `Set.difference` liveCells board)
  where
    birthCandidates =
      Set.fromList
        [ neighbor
        | livingCell <- Set.toList (liveCells board),
          neighbor <- neighbors livingCell
        ]
    isBirthCell cell = liveNeighbors board cell `elem` birthWhenNeighborsAre rules

nextBoardWithRules :: Rules -> Board -> Board
nextBoardWithRules rules board =
  setLiveCells board (survivors rules board `Set.union` births rules board)

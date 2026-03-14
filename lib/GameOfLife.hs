module GameOfLife
  ( Cell,
    Board (..),
    isAlive,
    makeBoard,
    nextGeneration
  )
where

import qualified Data.Set as Set

type Cell = (Int, Int)

newtype Board = Board
  { liveCells :: Set.Set Cell
  }

makeBoard :: [Cell] -> Board
makeBoard boardCells =
  Board
    { liveCells = Set.fromList boardCells
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

isAlive :: Board -> Cell -> Bool
isAlive board targetCell = targetCell `Set.member` liveCells board

liveNeighbors :: Board -> Cell -> Int
liveNeighbors board cell =
  length [neighbor | neighbor <- neighbors cell, isAlive board neighbor]

survivors :: Board -> Set.Set Cell
survivors board =
  Set.filter survives (liveCells board)
  where
    survives cell =
      let n = liveNeighbors board cell
       in n == 2 || n == 3

births :: Board -> Set.Set Cell
births board =
  Set.filter isBirthCell (birthCandidates `Set.difference` liveCells board)
  where
    birthCandidates =
      Set.fromList
        [ neighbor
        | livingCell <- Set.toList (liveCells board),
          neighbor <- neighbors livingCell
        ]
    isBirthCell cell = liveNeighbors board cell == 3

nextGeneration :: Board -> Board
nextGeneration board =
  board
    { liveCells = survivors board `Set.union` births board
    }

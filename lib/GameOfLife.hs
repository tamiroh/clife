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

data Board = Board
  { width :: Int,
    height :: Int,
    liveCells :: Set.Set Cell
  }

makeBoard :: Int -> Int -> [Cell] -> Maybe Board
makeBoard boardWidth boardHeight boardCells
  | boardWidth <= 0 = Nothing
  | boardHeight <= 0 = Nothing
  | all inBounds boardCells =
      Just
        Board
          { width = boardWidth,
            height = boardHeight,
            liveCells = Set.fromList boardCells
          }
  | otherwise = Nothing
  where
    inBounds (x, y) =
      x >= 0 && x < boardWidth && y >= 0 && y < boardHeight

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

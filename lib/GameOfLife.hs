module GameOfLife
  ( Cell,
    Board (..),
    isAlive,
    makeBoard,
    nextGeneration
  )
where

import Data.List (nub)

type Cell = (Int, Int)

data Board = Board
  { width :: Int,
    height :: Int,
    cells :: [Cell]
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
            cells = boardCells
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
isAlive board targetCell = targetCell `elem` cells board

liveNeighbors :: Board -> Cell -> Int
liveNeighbors board cell =
  length [neighbor | neighbor <- neighbors cell, isAlive board neighbor]

survivors :: Board -> [Cell]
survivors board =
  [cell | cell <- cells board, let n = liveNeighbors board cell, n == 2 || n == 3]

births :: Board -> [Cell]
births board =
  [ cell
  | cell <- nub [neighbor | livingCell <- cells board, neighbor <- neighbors livingCell]
  , not (isAlive board cell)
  , let n = liveNeighbors board cell
  , n == 3
  ]

nextGeneration :: Board -> Board
nextGeneration board =
  board
    { cells = survivors board ++ births board
    }

module Board
  ( Cell,
    Board,
    liveCells,
    isAlive,
    makeBoard,
    setLiveCells,
    advanceBoard,
  )
where

import qualified Data.Set as Set

type Cell = (Int, Int)

data Board = Board
  { nextBoard :: Board -> Board,
    liveCells :: Set.Set Cell
  }

makeBoard :: (Board -> Board) -> [Cell] -> Board
makeBoard mapToNextFn boardCells =
  Board
    { nextBoard = mapToNextFn,
      liveCells = Set.fromList boardCells
    }

isAlive :: Board -> Cell -> Bool
isAlive board targetCell = targetCell `Set.member` liveCells board

setLiveCells :: Board -> Set.Set Cell -> Board
setLiveCells board nextLiveCells =
  board
    { liveCells = nextLiveCells
    }

advanceBoard :: Board -> Board
advanceBoard board = nextBoard board board

module TerminalUI
  ( animateGenerations
  )
where

import Control.Concurrent (threadDelay)
import GameOfLife (Board, Cell, isAlive, nextGeneration)
import System.IO (hFlush, stdout)

viewportWidth :: Int
viewportWidth = 40

viewportHeight :: Int
viewportHeight = 20

showCell :: Board -> Cell -> Cell -> String
showCell board viewportOrigin cell
  | isAlive board (originX + x, originY + y) = "██"
  | otherwise = "  "
  where
    (originX, originY) = viewportOrigin
    (x, y) = cell

showBoard :: Board -> Cell -> String
showBoard board viewportOrigin =
  unlines $
    [topBorder]
      ++ [showRow y | y <- [0 .. viewportHeight - 1]]
      ++ [bottomBorder]
  where
    topBorder = "+" ++ replicate (viewportWidth * 2) '-' ++ "+"
    bottomBorder = topBorder
    showRow y = "|" ++ concat [showCell board viewportOrigin (x, y) | x <- [0 .. viewportWidth - 1]] ++ "|"

clearConsole :: IO ()
clearConsole = putStr "\ESC[2J\ESC[H"

moveCursorHome :: IO ()
moveCursorHome = putStr "\ESC[H"

hideCursor :: IO ()
hideCursor = putStr "\ESC[?25l"

showCursor :: IO ()
showCursor = putStr "\ESC[?25h"

animateGenerations :: Maybe Int -> Int -> Board -> IO ()
animateGenerations generationLimit delayInMicroseconds board = do
  clearConsole
  hideCursor
  mapM_ showGeneration numberedBoards
  showCursor
  where
    boards =
      case generationLimit of
        Just count -> take (count + 1) (iterate nextGeneration board)
        Nothing -> iterate nextGeneration board
    numberedBoards = zip ([0 ..] :: [Int]) boards
    showGeneration (generation, currentBoard) = do
      moveCursorHome
      putStrLn $ "Generation " ++ show generation
      putStrLn $ "Viewport: " ++ show viewportWidth ++ " x " ++ show viewportHeight
      putStrLn $ showBoard currentBoard (0, 0)
      hFlush stdout
      threadDelay delayInMicroseconds

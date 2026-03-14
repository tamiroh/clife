module TerminalUI
  ( animateGenerations
  )
where

import Control.Concurrent (threadDelay)
import GameOfLife (Board, Cell, height, isAlive, nextGeneration, width)
import System.IO (hFlush, stdout)

showCell :: Board -> Cell -> String
showCell board cell
  | isAlive board cell = "██"
  | otherwise = "  "

showBoard :: Board -> String
showBoard board =
  unlines $
    [topBorder]
      ++ [showRow y | y <- [0 .. height board - 1]]
      ++ [bottomBorder]
  where
    topBorder = "+" ++ replicate (width board * 2) '-' ++ "+"
    bottomBorder = topBorder
    showRow y = "|" ++ concat [showCell board (x, y) | x <- [0 .. width board - 1]] ++ "|"

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
      putStrLn $ "Board: " ++ show (width currentBoard) ++ " x " ++ show (height currentBoard)
      putStrLn $ showBoard currentBoard
      hFlush stdout
      threadDelay delayInMicroseconds

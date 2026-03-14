module TerminalUI
  ( animateGenerations
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import GameOfLife (Board, Cell, isAlive, nextGeneration)
import System.IO
  ( hFlush,
    stdout
  )
import TerminalControl
  ( Direction (..),
    clearConsole,
    hideCursor,
    moveCursorHome,
    readArrowKey,
    showCursor,
    withRawTerminalInput
  )

viewportWidth :: Int
viewportWidth = 40

viewportHeight :: Int
viewportHeight = 20

showCell :: Board -> Cell -> Cell -> Cell -> String
showCell board viewportOrigin cursor cell
  | (originX + x, originY + y) == cursor = highlight contents
  | otherwise = contents
  where
    contents
      | isAlive board (originX + x, originY + y) = "██"
      | otherwise = "  "
    (originX, originY) = viewportOrigin
    (x, y) = cell

highlight :: String -> String
highlight contents = "\ESC[7m" ++ contents ++ "\ESC[0m"

showBoard :: Board -> Cell -> Cell -> String
showBoard board viewportOrigin cursor =
  unlines $
    [topBorder]
      ++ [showRow y | y <- [0 .. viewportHeight - 1]]
      ++ [bottomBorder]
  where
    topBorder = "+" ++ replicate (viewportWidth * 2) '-' ++ "+"
    bottomBorder = topBorder
    showRow y = "|" ++ concat [showCell board viewportOrigin cursor (x, y) | x <- [0 .. viewportWidth - 1]] ++ "|"

animateGenerations :: Maybe Int -> Int -> Board -> IO ()
animateGenerations generationLimit delayInMicroseconds board = do
  clearConsole
  withRawTerminalInput $
    (hideCursor >> runLoop generationLimit delayInMicroseconds 0 (0, 0) (0, 0) board)
      `finally` showCursor

runLoop :: Maybe Int -> Int -> Int -> Cell -> Cell -> Board -> IO ()
runLoop generationLimit delayInMicroseconds generation viewportOrigin cursor currentBoard = do
  moveCursorHome
  putStrLn $ "Generation " ++ show generation
  putStrLn $
    "Viewport: "
      ++ show viewportWidth
      ++ " x "
      ++ show viewportHeight
      ++ " @ "
      ++ show viewportOrigin
  putStrLn $ "Cursor: " ++ show cursor
  putStrLn $ showBoard currentBoard viewportOrigin cursor
  hFlush stdout
  (nextViewportOrigin, nextCursor) <- waitForNextFrame delayInMicroseconds viewportOrigin cursor
  case generationLimit of
    Just count | generation >= count -> pure ()
    _ -> runLoop generationLimit delayInMicroseconds (generation + 1) nextViewportOrigin nextCursor (nextGeneration currentBoard)

waitForNextFrame :: Int -> Cell -> Cell -> IO (Cell, Cell)
waitForNextFrame remainingMicroseconds viewportOrigin cursor
  | remainingMicroseconds <= 0 = pure (viewportOrigin, cursor)
  | otherwise = do
      (nextViewportOrigin, nextCursor) <- getNextViewState viewportOrigin cursor
      threadDelay (min 10000 remainingMicroseconds)
      waitForNextFrame (remainingMicroseconds - min 10000 remainingMicroseconds) nextViewportOrigin nextCursor

getNextViewState :: Cell -> Cell -> IO (Cell, Cell)
getNextViewState viewportOrigin cursor = do
  maybeDirection <- readArrowKey
  let (nextViewportOrigin, nextCursor) =
        case maybeDirection of
          Just direction -> applyDirection viewportOrigin cursor direction
          Nothing -> (viewportOrigin, cursor)
  pure (nextViewportOrigin, nextCursor)

moveCursor :: Cell -> Direction -> Cell
moveCursor (x, y) direction =
  case direction of
    MoveUp -> (x, y - 1)
    MoveDown -> (x, y + 1)
    MoveLeft -> (x - 1, y)
    MoveRight -> (x + 1, y)

applyDirection :: Cell -> Cell -> Direction -> (Cell, Cell)
applyDirection viewportOrigin cursor direction =
  (nextViewportOrigin, nextCursor)
  where
    nextCursor = moveCursor cursor direction
    nextViewportOrigin = adjustViewportOrigin viewportOrigin nextCursor
    adjustViewportOrigin (originX, originY) (cursorX, cursorY) =
      (adjust originX cursorX viewportWidth, adjust originY cursorY viewportHeight)
      where
        adjust origin cursorCoord viewportSize
          | cursorCoord < origin = cursorCoord
          | cursorCoord >= origin + viewportSize = cursorCoord - viewportSize + 1
          | otherwise = origin

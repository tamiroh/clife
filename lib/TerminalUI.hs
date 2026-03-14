module TerminalUI
  ( animateGenerations,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import qualified Data.Set as Set
import GameOfLife (Board (..), Cell, isAlive, nextGeneration)
import System.IO
  ( hFlush,
    stdout,
  )
import TerminalControl
  ( Direction (..),
    Input (..),
    clearConsole,
    clearFromCursorDown,
    hideCursor,
    moveCursorHome,
    readInput,
    showCursor,
    withRawTerminalInput,
  )

viewportWidth :: Int
viewportWidth = 40

viewportHeight :: Int
viewportHeight = 20

maxMiniMapWidth :: Int
maxMiniMapWidth = 20

maxMiniMapHeight :: Int
maxMiniMapHeight = 10

data ViewState = ViewState
  { isRunning :: Bool,
    generation :: Int,
    viewViewportOrigin :: Cell,
    viewCursor :: Cell,
    viewBoard :: Board
  }

data RunConfiguration = RunConfiguration
  { generationLimit :: Maybe Int,
    delayInMicroseconds :: Int
  }

viewportCells :: Cell -> [Cell]
viewportCells (viewportX, viewportY) =
  [ (x, y)
  | x <- [viewportX .. viewportX + viewportWidth - 1],
    y <- [viewportY .. viewportY + viewportHeight - 1]
  ]

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

highlightMiniMap :: String -> String
highlightMiniMap contents = "\ESC[48;5;238m" ++ contents ++ "\ESC[0m"

showBoardLines :: Board -> Cell -> Cell -> [String]
showBoardLines board viewportOrigin cursor =
  [topBorder]
    ++ [showRow y | y <- [0 .. viewportHeight - 1]]
    ++ [bottomBorder]
  where
    topBorder = "+" ++ replicate (viewportWidth * 2) '-' ++ "+"
    bottomBorder = topBorder
    showRow y = "|" ++ concat [showCell board viewportOrigin cursor (x, y) | x <- [0 .. viewportWidth - 1]] ++ "|"

showMiniMapLines :: Board -> Cell -> [String]
showMiniMapLines board viewportOrigin
  | Set.null (liveCells board) = ["(empty)"]
  | otherwise = [showMiniMapRow y | y <- [0 .. miniMapHeight - 1]]
  where
    boardCells = Set.toList (liveCells board)
    ((minX, minY), (spanX, spanY)) = miniMapBounds boardCells viewportOrigin
    (miniMapWidth, miniMapHeight) = miniMapSize spanX spanY
    scaleCell (x, y) =
      ( scaleCoordinate x minX spanX miniMapWidth,
        scaleCoordinate y minY spanY miniMapHeight
      )
    scaledCells =
      Set.fromList (map scaleCell boardCells)
    scaledViewportCells =
      Set.fromList (map scaleCell (viewportCells viewportOrigin))
    showMiniMapRow y =
      concat
        [ showMiniMapCell x y scaledCells scaledViewportCells
        | x <- [0 .. miniMapWidth - 1]
        ]

showMiniMapCell :: Int -> Int -> Set.Set Cell -> Set.Set Cell -> String
showMiniMapCell x y scaledCells scaledViewportCells
  | (x, y) `Set.member` scaledViewportCells = highlightMiniMap contents
  | otherwise = contents
  where
    contents
      | (x, y) `Set.member` scaledCells = "#"
      | otherwise = "."

miniMapBounds :: [Cell] -> Cell -> (Cell, Cell)
miniMapBounds boardCells (viewportX, viewportY) =
  ((minX, minY), (spanX, spanY))
  where
    xs = viewportX : map fst boardCells
    ys = viewportY : map snd boardCells
    maxXs = (viewportX + viewportWidth - 1) : xs
    maxYs = (viewportY + viewportHeight - 1) : ys
    minX = minimum xs
    minY = minimum ys
    maxX = maximum maxXs
    maxY = maximum maxYs
    spanX = max 1 (maxX - minX + 1)
    spanY = max 1 (maxY - minY + 1)

scaleCoordinate :: Int -> Int -> Int -> Int -> Int
scaleCoordinate value minValue spanValue targetSize =
  ((value - minValue) * max 0 (targetSize - 1)) `div` spanValue

miniMapSize :: Int -> Int -> (Int, Int)
miniMapSize spanX spanY
  | spanX * maxMiniMapHeight >= spanY * maxMiniMapWidth =
      (maxMiniMapWidth, max 1 (ceilingDiv (spanY * maxMiniMapWidth) spanX))
  | otherwise =
      (max 1 (ceilingDiv (spanX * maxMiniMapHeight) spanY), maxMiniMapHeight)

ceilingDiv :: Int -> Int -> Int
ceilingDiv numerator denominator =
  (numerator + denominator - 1) `div` denominator

padLines :: Int -> [String] -> [String]
padLines targetLength rows =
  rows ++ replicate (targetLength - length rows) ""

showLayout :: Board -> Cell -> Cell -> String
showLayout board viewportOrigin cursor =
  unlines $
    zipWith
      (\boardRow miniMapRow -> boardRow ++ "    " ++ miniMapRow)
      paddedBoardLines
      paddedMiniMapLines
  where
    boardLines = showBoardLines board viewportOrigin cursor
    miniMapLines = "Mini-map:" : showMiniMapLines board viewportOrigin
    totalRows = max (length boardLines) (length miniMapLines)
    paddedBoardLines = padLines totalRows boardLines
    paddedMiniMapLines = padLines totalRows miniMapLines

animateGenerations :: Maybe Int -> Int -> Board -> IO ()
animateGenerations maybeGenerationLimit frameDelayInMicroseconds board = do
  clearConsole
  withRawTerminalInput $
    (hideCursor >> runLoop runConfiguration initialViewState)
      `finally` showCursor
  where
    runConfiguration =
      RunConfiguration
        { generationLimit = maybeGenerationLimit,
          delayInMicroseconds = frameDelayInMicroseconds
        }
    initialViewState =
      ViewState
        { isRunning = True,
          generation = 0,
          viewViewportOrigin = (0, 0),
          viewCursor = (0, 0),
          viewBoard = board
        }

runLoop :: RunConfiguration -> ViewState -> IO ()
runLoop runConfiguration viewState = do
  moveCursorHome
  clearFromCursorDown
  putStrLn $ "Generation " ++ show (generation viewState)
  putStrLn $ "Status: " ++ if isRunning viewState then "running" else "paused"
  putStrLn $ showLayout (viewBoard viewState) (viewViewportOrigin viewState) (viewCursor viewState)
  putStrLn "  [Arrow keys] Move cursor  [Space] Run / Pause"
  hFlush stdout
  nextViewState <- waitForNextFrame (delayInMicroseconds runConfiguration) viewState
  case generationLimit runConfiguration of
    Just count | generation viewState >= count -> pure ()
    _ -> runLoop runConfiguration (advanceBoard nextViewState)

waitForNextFrame :: Int -> ViewState -> IO ViewState
waitForNextFrame remainingMicroseconds viewState
  | remainingMicroseconds <= 0 = pure viewState
  | otherwise = do
      nextViewState <- getNextViewState viewState
      threadDelay (min 10000 remainingMicroseconds)
      waitForNextFrame (remainingMicroseconds - min 10000 remainingMicroseconds) nextViewState

getNextViewState :: ViewState -> IO ViewState
getNextViewState viewState = do
  maybeInput <- readInput
  pure $
    case maybeInput of
      Just (MoveCursor direction) -> applyDirection viewState direction
      Just ToggleRunning -> viewState {isRunning = not (isRunning viewState)}
      Nothing -> viewState

moveCursor :: Cell -> Direction -> Cell
moveCursor (x, y) direction =
  case direction of
    MoveUp -> (x, y - 1)
    MoveDown -> (x, y + 1)
    MoveLeft -> (x - 1, y)
    MoveRight -> (x + 1, y)

applyDirection :: ViewState -> Direction -> ViewState
applyDirection viewState direction =
  viewState
    { viewViewportOrigin = nextViewportOrigin,
      viewCursor = nextCursor
    }
  where
    nextCursor = moveCursor (viewCursor viewState) direction
    nextViewportOrigin = (adjust originX cursorX viewportWidth, adjust originY cursorY viewportHeight)
    (originX, originY) = viewViewportOrigin viewState
    (cursorX, cursorY) = nextCursor
    adjust origin cursorCoord viewportSize
      | cursorCoord < origin = cursorCoord
      | cursorCoord >= origin + viewportSize = cursorCoord - viewportSize + 1
      | otherwise = origin

advanceBoard :: ViewState -> ViewState
advanceBoard viewState
  | isRunning viewState =
      viewState
        { generation = generation viewState + 1,
          viewBoard = nextGeneration (viewBoard viewState)
        }
  | otherwise = viewState

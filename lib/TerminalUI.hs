module TerminalUI
  ( animateGenerations,
  )
where

import Board (Board, Cell, toggleCell)
import qualified Board
import Control.Concurrent (threadDelay)
import Control.Exception (finally)
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
import TerminalRender (renderLayout, viewportHeight, viewportWidth)

data ViewState = ViewState
  { isRunning :: Bool,
    generation :: Int,
    viewportOrigin :: Cell,
    cursor :: Cell,
    board :: Board
  }

data RunConfiguration = RunConfiguration
  { generationLimit :: Maybe Int,
    delayInMicroseconds :: Int
  }

animateGenerations :: Maybe Int -> Int -> Board -> IO ()
animateGenerations maybeGenerationLimit frameDelayInMicroseconds initialBoard = do
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
          viewportOrigin = (0, 0),
          cursor = (0, 0),
          board = initialBoard
        }

runLoop :: RunConfiguration -> ViewState -> IO ()
runLoop runConfiguration viewState = do
  moveCursorHome
  clearFromCursorDown
  putStrLn $ "Generation " ++ show (generation viewState)
  putStrLn $ "Status: " ++ if isRunning viewState then "running" else "paused"
  putStrLn $ renderLayout (board viewState) (viewportOrigin viewState) (cursor viewState)
  putStrLn "  [Arrow keys] Move cursor  [X] Toggle cell  [Space] Run / Pause  [Q] Quit"
  hFlush stdout
  maybeNextViewState <- waitForNextFrame (delayInMicroseconds runConfiguration) viewState
  case (generationLimit runConfiguration, maybeNextViewState) of
    (_, Nothing) -> pure ()
    (Just count, _) | generation viewState >= count -> pure ()
    (_, Just nextViewState) -> runLoop runConfiguration (advanceBoard nextViewState)

waitForNextFrame :: Int -> ViewState -> IO (Maybe ViewState)
waitForNextFrame remainingMicroseconds viewState
  | remainingMicroseconds <= 0 = pure (Just viewState)
  | otherwise = do
      maybeNextViewState <- getNextViewState viewState
      case maybeNextViewState of
        Nothing -> pure Nothing
        Just nextViewState -> do
          threadDelay (min 10000 remainingMicroseconds)
          waitForNextFrame (remainingMicroseconds - min 10000 remainingMicroseconds) nextViewState

getNextViewState :: ViewState -> IO (Maybe ViewState)
getNextViewState viewState = do
  maybeInput <- readInput
  pure $
    case maybeInput of
      Just (MoveCursor direction) -> Just (applyDirection viewState direction)
      Just ToggleCell -> Just (toggleCursorCell viewState)
      Just ToggleRunning -> Just viewState {isRunning = not (isRunning viewState)}
      Just Quit -> Nothing
      Nothing -> Just viewState

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
    { viewportOrigin = nextViewportOrigin,
      cursor = nextCursor
    }
  where
    nextCursor = moveCursor (cursor viewState) direction
    nextViewportOrigin = (adjust originX cursorX viewportWidth, adjust originY cursorY viewportHeight)
    (originX, originY) = viewportOrigin viewState
    (cursorX, cursorY) = nextCursor
    adjust origin cursorCoord viewportSize
      | cursorCoord < origin = cursorCoord
      | cursorCoord >= origin + viewportSize = cursorCoord - viewportSize + 1
      | otherwise = origin

toggleCursorCell :: ViewState -> ViewState
toggleCursorCell viewState =
  viewState
    { board = toggleCell (board viewState) (cursor viewState)
    }

advanceBoard :: ViewState -> ViewState
advanceBoard viewState
  | isRunning viewState =
      viewState
        { generation = generation viewState + 1,
          board = Board.advanceBoard (board viewState)
        }
  | otherwise = viewState

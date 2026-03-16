module TerminalUI
  ( animateGenerations,
  )
where

import Board (Board, Cell, toggleCell)
import qualified Board
import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import System.Console.ANSI (getTerminalSize)
import System.IO
  ( hFlush,
    stdout,
  )
import TerminalControl
  ( Direction (..),
    Input (..),
    clearConsole,
    clearLine,
    hideCursor,
    moveCursorHome,
    readInput,
    showCursor,
    withRawTerminalInput,
  )
import TerminalRender
  ( miniMapSizeFor,
    miniMapTargetCellFor,
    renderLayout,
  )

data ViewState = ViewState
  { isRunning :: Bool,
    isJumpMode :: Bool,
    jumpCursor :: Cell,
    viewportSize :: (Int, Int),
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
          isJumpMode = False,
          jumpCursor = (0, 0),
          viewportSize = (40, 20),
          generation = 0,
          viewportOrigin = (0, 0),
          cursor = (0, 0),
          board = initialBoard
        }

runLoop :: RunConfiguration -> ViewState -> IO ()
runLoop runConfiguration viewState = do
  nextViewportSize <- refreshViewportSize viewState
  let nextViewState = viewState {viewportSize = nextViewportSize}
  moveCursorHome
  putLine $
    "Generation "
      ++ show (generation nextViewState)
      ++ "  Status: "
      ++ (if isRunning nextViewState then "running" else "paused")
      ++ "  Mode: "
      ++ (if isJumpMode nextViewState then "jump" else "normal")
  mapM_ putLine $
    lines $
    renderLayout
      nextViewportSize
      (board nextViewState)
      (viewportOrigin nextViewState)
      (cursor nextViewState)
      (if isJumpMode nextViewState then Just (jumpCursor nextViewState) else Nothing)
  putLine "  [Arrow keys] Move cursor  [WASD] Move view  [X] Toggle cell  [Space] Run / Pause"
  putLine "  [G] Jump mode  [Enter] Confirm jump"
  putLine "  [Q] Quit"
  hFlush stdout
  maybeAdvancedViewState <- waitForNextFrame nextViewportSize (delayInMicroseconds runConfiguration) nextViewState
  case (generationLimit runConfiguration, maybeAdvancedViewState) of
    (_, Nothing) -> pure ()
    (Just count, _) | generation nextViewState >= count -> pure ()
    (_, Just advancedViewState) -> runLoop runConfiguration (advanceBoard advancedViewState)
  where
    putLine line = clearLine >> putStrLn line

waitForNextFrame :: (Int, Int) -> Int -> ViewState -> IO (Maybe ViewState)
waitForNextFrame activeViewportSize remainingMicroseconds viewState
  | remainingMicroseconds <= 0 = pure (Just viewState)
  | otherwise = do
      maybeNextViewState <- getNextViewState activeViewportSize viewState
      case maybeNextViewState of
        Nothing -> pure Nothing
        Just nextViewState -> do
          threadDelay (min 10000 remainingMicroseconds)
          waitForNextFrame activeViewportSize (remainingMicroseconds - min 10000 remainingMicroseconds) nextViewState

getNextViewState :: (Int, Int) -> ViewState -> IO (Maybe ViewState)
getNextViewState activeViewportSize viewState = do
  maybeInput <- readInput
  pure $
    case maybeInput of
      Just (MoveCursor direction) -> Just (applyDirectionalInput activeViewportSize viewState applyDirection direction)
      Just (MoveViewport direction) -> Just (applyDirectionalInput activeViewportSize viewState applyViewportDirection direction)
      Just ToggleCell -> Just (toggleCursorCell viewState)
      Just ToggleRunning -> Just viewState {isRunning = not (isRunning viewState)}
      Just ToggleJumpMode -> Just viewState {isJumpMode = not (isJumpMode viewState)}
      Just ConfirmJump -> Just (applyIfJumpMode viewState (applyJump activeViewportSize))
      Just Quit -> Nothing
      Nothing -> Just viewState

applyDirectionalInput :: (Int, Int) -> ViewState -> ((Int, Int) -> ViewState -> Direction -> ViewState) -> Direction -> ViewState
applyDirectionalInput activeViewportSize viewState applyInNormalMode direction
  | isJumpMode viewState = moveJumpCursor activeViewportSize viewState direction
  | otherwise = applyInNormalMode activeViewportSize viewState direction

applyIfJumpMode :: ViewState -> (ViewState -> ViewState) -> ViewState
applyIfJumpMode viewState transform
  | isJumpMode viewState = transform viewState
  | otherwise = viewState

moveCursor :: Cell -> Direction -> Cell
moveCursor (x, y) direction =
  case direction of
    MoveUp -> (x, y - 1)
    MoveDown -> (x, y + 1)
    MoveLeft -> (x - 1, y)
    MoveRight -> (x + 1, y)

applyDirection :: (Int, Int) -> ViewState -> Direction -> ViewState
applyDirection (viewportWidth, viewportHeight) viewState direction =
  viewState
    { viewportOrigin = nextViewportOrigin,
      cursor = nextCursor
    }
  where
    nextCursor = moveCursor (cursor viewState) direction
    nextViewportOrigin = (adjust originX cursorX viewportWidth, adjust originY cursorY viewportHeight)
    (originX, originY) = viewportOrigin viewState
    (cursorX, cursorY) = nextCursor
    adjust origin cursorCoord viewportAxisSize
      | cursorCoord < origin = cursorCoord
      | cursorCoord >= origin + viewportAxisSize = cursorCoord - viewportAxisSize + 1
      | otherwise = origin

toggleCursorCell :: ViewState -> ViewState
toggleCursorCell viewState =
  viewState
    { board = toggleCell (board viewState) (cursor viewState)
    }

applyViewportDirection :: (Int, Int) -> ViewState -> Direction -> ViewState
applyViewportDirection _ viewState direction =
  viewState
    { viewportOrigin = moveCursor (viewportOrigin viewState) direction,
      cursor = moveCursor (cursor viewState) direction
    }

moveJumpCursor :: (Int, Int) -> ViewState -> Direction -> ViewState
moveJumpCursor activeViewportSize viewState direction =
  viewState
    { jumpCursor = (clamp 0 (miniMapWidth - 1) nextX, clamp 0 (miniMapHeight - 1) nextY)
    }
  where
    (miniMapWidth, miniMapHeight) = miniMapSizeFor activeViewportSize (board viewState) (viewportOrigin viewState)
    (nextX, nextY) = moveCursor (jumpCursor viewState) direction
    clamp lower upper value = max lower (min upper value)

applyJump :: (Int, Int) -> ViewState -> ViewState
applyJump (viewportWidth, viewportHeight) viewState =
  viewState
    { isJumpMode = False,
      viewportOrigin = (targetX - viewportWidth `div` 2, targetY - viewportHeight `div` 2),
      cursor = targetCell
    }
  where
    targetCell@(targetX, targetY) =
      miniMapTargetCellFor
        (viewportWidth, viewportHeight)
        (board viewState)
        (viewportOrigin viewState)
        (jumpCursor viewState)

refreshViewportSize :: ViewState -> IO (Int, Int)
refreshViewportSize viewState = do
  maybeWindow <- getTerminalSize
  pure $
    case maybeWindow of
      Just window -> viewportSizeForWindow window
      Nothing -> viewportSize viewState

viewportSizeForWindow :: (Int, Int) -> (Int, Int)
viewportSizeForWindow window =
  case window of
    (rows, columns) ->
      ( max 20 ((columns - 28 - 8) `div` 2),
        max 10 (rows - 7)
      )

advanceBoard :: ViewState -> ViewState
advanceBoard viewState
  | isRunning viewState =
      viewState
        { generation = generation viewState + 1,
          board = Board.advanceBoard (board viewState)
        }
  | otherwise = viewState

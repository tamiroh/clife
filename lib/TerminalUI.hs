module TerminalUI
  ( RunConfig (..),
    animateGenerations,
  )
where

import Board (Board, Cell, toggleCell)
import qualified Board
import Brick
  ( App (..),
    BrickEvent (AppEvent, VtyEvent),
    EventM,
    Widget,
    attrMap,
    getVtyHandle,
    halt,
    neverShowCursor,
    str,
    vBox,
  )
import Brick.BChan (newBChan, writeBChan)
import Brick.Main (customMain)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, modify, put)
import Data.Char (toLower)
import Graphics.Vty
  ( Event (EvKey, EvResize),
    Key (KChar, KDown, KEnter, KLeft, KRight, KUp),
    defAttr,
    defaultConfig,
    displayBounds,
    outputIface,
  )
import Graphics.Vty.CrossPlatform (mkVty)
import TerminalRender
  ( miniMapSizeFor,
    miniMapTargetCellFor,
    renderLayout,
  )

----------------------------------------------------------------------
-- State
----------------------------------------------------------------------

data Simulation = Simulation
  { generation :: Int,
    board :: Board
  }

advanceGeneration :: Simulation -> Simulation
advanceGeneration currentSimulation =
  currentSimulation
    { generation = generation currentSimulation + 1,
      board = Board.advanceBoard (board currentSimulation)
    }

toggleCellAt :: Cell -> Simulation -> Simulation
toggleCellAt cell currentSimulation = currentSimulation {board = toggleCell (board currentSimulation) cell}

data ViewState = ViewState
  { isRunning :: Bool,
    isJumpMode :: Bool,
    jumpCursor :: Cell,
    viewportSize :: (Int, Int),
    generationLimit :: Maybe Int,
    viewportOrigin :: Cell,
    cursor :: Cell,
    simulation :: Simulation
  }

----------------------------------------------------------------------
-- Key action
----------------------------------------------------------------------

data Direction
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight

data Action
  = MoveCursorAction Direction
  | MoveViewportAction Direction
  | ToggleCellAction
  | ToggleRunningAction
  | ToggleJumpModeAction
  | ConfirmJumpAction

keyToAction :: Key -> Maybe Action
keyToAction key =
  case key of
    KChar c | toLower c == 'w' -> Just (MoveViewportAction MoveUp)
    KChar c | toLower c == 's' -> Just (MoveViewportAction MoveDown)
    KChar c | toLower c == 'a' -> Just (MoveViewportAction MoveLeft)
    KChar c | toLower c == 'd' -> Just (MoveViewportAction MoveRight)
    KChar c | toLower c == 'x' -> Just ToggleCellAction
    KChar c | toLower c == 'g' -> Just ToggleJumpModeAction
    KChar ' ' -> Just ToggleRunningAction
    KEnter -> Just ConfirmJumpAction
    KUp -> Just (MoveCursorAction MoveUp)
    KDown -> Just (MoveCursorAction MoveDown)
    KLeft -> Just (MoveCursorAction MoveLeft)
    KRight -> Just (MoveCursorAction MoveRight)
    _ -> Nothing

applyAction :: ViewState -> Action -> ViewState
applyAction viewState action =
  case action of
    MoveCursorAction direction -> applyDirectionalInput activeViewportSize viewState applyDirection direction
    MoveViewportAction direction -> applyDirectionalInput activeViewportSize viewState applyViewportDirection direction
    ToggleCellAction -> viewState {simulation = toggleCellAt (cursor viewState) (simulation viewState)}
    ToggleRunningAction -> viewState {isRunning = not (isRunning viewState)}
    ToggleJumpModeAction -> viewState {isJumpMode = not (isJumpMode viewState)}
    ConfirmJumpAction
      | isJumpMode viewState -> applyJump activeViewportSize viewState
      | otherwise -> viewState
  where
    activeViewportSize = viewportSize viewState

applyDirectionalInput :: (Int, Int) -> ViewState -> ((Int, Int) -> ViewState -> Direction -> ViewState) -> Direction -> ViewState
applyDirectionalInput activeViewportSize viewState applyInNormalMode direction
  | isJumpMode viewState = moveJumpCursor activeViewportSize viewState direction
  | otherwise = applyInNormalMode activeViewportSize viewState direction

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
    (miniMapWidth, miniMapHeight) = miniMapSizeFor activeViewportSize (board (simulation viewState)) (viewportOrigin viewState)
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
        (board (simulation viewState))
        (viewportOrigin viewState)
        (jumpCursor viewState)

----------------------------------------------------------------------
-- View
----------------------------------------------------------------------

drawUI :: ViewState -> [Widget ()]
drawUI viewState =
  [ vBox
      [ str statusLine,
        renderLayout (viewportSize viewState) (board (simulation viewState)) (viewportOrigin viewState) (cursor viewState) maybeJumpCursor,
        str "  [Arrow keys] Move cursor  [WASD] Move view  [X] Toggle cell  [Space] Run / Pause",
        str "  [G] Jump mode  [Enter] Confirm jump",
        str "  [Q] Quit"
      ]
  ]
  where
    statusLine =
      "Generation "
        ++ show (generation (simulation viewState))
        ++ "  Status: "
        ++ (if isRunning viewState then "running" else "paused")
        ++ "  Mode: "
        ++ (if isJumpMode viewState then "jump" else "normal")
    maybeJumpCursor = if isJumpMode viewState then Just (jumpCursor viewState) else Nothing

----------------------------------------------------------------------
-- App wiring
----------------------------------------------------------------------

data ClifeEvent = Tick

data RunConfig = RunConfig
  { runGenerationLimit :: Maybe Int,
    runFrameDelayMicroseconds :: Int
  }

animateGenerations :: RunConfig -> Board -> IO ()
animateGenerations runConfig initialBoard = do
  eventChannel <- newBChan 10
  _ <- forkIO $ forever $ do
    threadDelay (runFrameDelayMicroseconds runConfig)
    writeBChan eventChannel Tick
  initialVty <- buildVty
  _ <- customMain initialVty buildVty (Just eventChannel) clifeApp initialViewState
  pure ()
  where
    buildVty = mkVty defaultConfig
    initialViewState =
      ViewState
        { isRunning = True,
          isJumpMode = False,
          jumpCursor = (0, 0),
          viewportSize = (40, 20),
          generationLimit = runGenerationLimit runConfig,
          viewportOrigin = (0, 0),
          cursor = (0, 0),
          simulation = Simulation {generation = 0, board = initialBoard}
        }

clifeApp :: App ViewState ClifeEvent ()
clifeApp =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleClifeEvent,
      appStartEvent = initializeViewportSize,
      appAttrMap = const (attrMap defAttr [])
    }

initializeViewportSize :: EventM () ViewState ()
initializeViewportSize = do
  vty <- getVtyHandle
  (width, height) <- liftIO (displayBounds (outputIface vty))
  modify (\viewState -> viewState {viewportSize = viewportSizeForWindow width height})

handleClifeEvent :: BrickEvent () ClifeEvent -> EventM () ViewState ()
handleClifeEvent (AppEvent Tick) = tick
handleClifeEvent (VtyEvent (EvResize width height)) =
  modify (\viewState -> viewState {viewportSize = viewportSizeForWindow width height})
handleClifeEvent (VtyEvent (EvKey (KChar c) _)) | toLower c == 'q' = halt
handleClifeEvent (VtyEvent (EvKey key _)) =
  case keyToAction key of
    Nothing -> pure ()
    Just action -> modify (`applyAction` action)
handleClifeEvent _ = pure ()

tick :: EventM () ViewState ()
tick = do
  viewState <- get
  when (isRunning viewState) $ do
    let nextViewState = viewState {simulation = advanceGeneration (simulation viewState)}
    put nextViewState
    case generationLimit nextViewState of
      Just limitCount | generation (simulation nextViewState) >= limitCount -> halt
      _ -> pure ()

viewportSizeForWindow :: Int -> Int -> (Int, Int)
viewportSizeForWindow width height =
  ( max 20 ((width - 28 - 8) `div` 2),
    max 10 (height - 7)
  )

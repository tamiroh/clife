module TerminalUI
  ( RunConfig (..),
    animateGenerations,
  )
where

import Board (Board, Cell, isAlive, liveCells, toggleCell)
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
    raw,
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
import qualified Data.Set as Set
import Graphics.Vty
  ( Attr,
    Color (Color240),
    Event (EvKey, EvResize),
    Image,
    Key (KChar, KDown, KEnter, KLeft, KRight, KUp),
    charFill,
    defAttr,
    defaultConfig,
    displayBounds,
    horizCat,
    outputIface,
    reverseVideo,
    string,
    vertCat,
    withBackColor,
    withForeColor,
    withStyle,
  )
import Graphics.Vty.CrossPlatform (mkVty)
import MiniMap
  ( miniMapProjectionFor,
    miniMapSize,
    projectCell,
    unprojectCell,
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
    (miniMapWidth, miniMapHeight) =
      miniMapSize (miniMapProjectionFor activeViewportSize (board (simulation viewState)) (viewportOrigin viewState))
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
      unprojectCell
        (miniMapProjectionFor (viewportWidth, viewportHeight) (board (simulation viewState)) (viewportOrigin viewState))
        (jumpCursor viewState)

----------------------------------------------------------------------
-- View
----------------------------------------------------------------------

liveAttr :: Attr
liveAttr = defAttr `withForeColor` Color240 56

viewportBgAttr :: Attr
viewportBgAttr = defAttr `withBackColor` Color240 222

liveViewportBgAttr :: Attr
liveViewportBgAttr = defAttr `withForeColor` Color240 56 `withBackColor` Color240 222

reverseAttr :: Attr
reverseAttr = defAttr `withStyle` reverseVideo

liveReverseAttr :: Attr
liveReverseAttr = defAttr `withForeColor` Color240 56 `withStyle` reverseVideo

attrFor :: Bool -> Attr
attrFor alive = if alive then liveAttr else defAttr

cursorAttrFor :: Bool -> Attr
cursorAttrFor alive = if alive then liveReverseAttr else reverseAttr

edgeHintDistance :: Int
edgeHintDistance = 100

drawUI :: ViewState -> [Widget ()]
drawUI viewState =
  [ vBox
      [ str statusLine,
        renderLayout viewState,
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

renderLayout :: ViewState -> Widget n
renderLayout viewState =
  raw (horizCat [boardImage, gapImage, miniMapImage])
  where
    currentBoard = board (simulation viewState)
    activeViewportSize = viewportSize viewState
    viewport = viewportOrigin viewState
    maybeJumpCursor = if isJumpMode viewState then Just (jumpCursor viewState) else Nothing
    boardImage = renderBoardImage activeViewportSize currentBoard viewport (cursor viewState)
    miniMapImage =
      vertCat (string defAttr "Mini-map:" : renderMiniMapImages activeViewportSize currentBoard viewport maybeJumpCursor)
    gapImage = charFill defAttr ' ' (4 :: Int) (1 :: Int)

viewportCells :: (Int, Int) -> Cell -> [Cell]
viewportCells (viewportWidth, viewportHeight) (viewportX, viewportY) =
  [ (x, y)
  | x <- [viewportX .. viewportX + viewportWidth - 1],
    y <- [viewportY .. viewportY + viewportHeight - 1]
  ]

renderCellImage :: Board -> Cell -> Cell -> Cell -> Image
renderCellImage renderedBoard viewport cursorPosition cell
  | (originX + x, originY + y) == cursorPosition = string (cursorAttrFor alive) contents
  | otherwise = string (attrFor alive) contents
  where
    (originX, originY) = viewport
    (x, y) = cell
    alive = isAlive renderedBoard (originX + x, originY + y)
    contents = if alive then "██" else "  "

renderBoardImage :: (Int, Int) -> Board -> Cell -> Cell -> Image
renderBoardImage (viewportWidth, viewportHeight) renderedBoard viewport cursorPosition =
  vertCat ([topBorder] ++ [showRow y | y <- [0 .. viewportHeight - 1]] ++ [bottomBorder])
  where
    (viewportX, viewportY) = viewport
    topBorder =
      horizCat $
        [string defAttr "+"]
          ++ [borderSegment (hasAliveCellAlong (aboveCells x)) | x <- [0 .. viewportWidth - 1]]
          ++ [string defAttr "+"]
    bottomBorder =
      horizCat $
        [string defAttr "+"]
          ++ [borderSegment (hasAliveCellAlong (belowCells x)) | x <- [0 .. viewportWidth - 1]]
          ++ [string defAttr "+"]
    showRow y =
      horizCat $
        [leftBorder y]
          ++ [renderCellImage renderedBoard viewport cursorPosition (x, y) | x <- [0 .. viewportWidth - 1]]
          ++ [rightBorder y]
    leftBorder y = edgeMarker (hasAliveCellAlong (leftCells y))
    rightBorder y = edgeMarker (hasAliveCellAlong (rightCells y))
    aboveCells x =
      [ (viewportX + x, viewportY - distance)
      | distance <- [1 .. edgeHintDistance]
      ]
    belowCells x =
      [ (viewportX + x, viewportY + viewportHeight + distance - 1)
      | distance <- [1 .. edgeHintDistance]
      ]
    leftCells y =
      [ (viewportX - distance, viewportY + y)
      | distance <- [1 .. edgeHintDistance]
      ]
    rightCells y =
      [ (viewportX + viewportWidth + distance - 1, viewportY + y)
      | distance <- [1 .. edgeHintDistance]
      ]
    hasAliveCellAlong = any (isAlive renderedBoard)
    borderSegment shouldHighlight = string (attrFor shouldHighlight) "--"
    edgeMarker shouldHighlight = string (attrFor shouldHighlight) "|"

renderMiniMapImages :: (Int, Int) -> Board -> Cell -> Maybe Cell -> [Image]
renderMiniMapImages activeViewportSize renderedBoard viewport maybeJumpCursor
  | Set.null (liveCells renderedBoard) = [string defAttr "(empty)"]
  | otherwise = [showMiniMapRow y | y <- [0 .. miniMapHeight - 1]]
  where
    projection = miniMapProjectionFor activeViewportSize renderedBoard viewport
    (miniMapWidth, miniMapHeight) = miniMapSize projection
    scaledCells = Set.fromList (map (projectCell projection) (Set.toList (liveCells renderedBoard)))
    scaledViewportCells = Set.fromList (map (projectCell projection) (viewportCells activeViewportSize viewport))
    showMiniMapRow y =
      horizCat
        [ showMiniMapCellImage
            x
            y
            scaledCells
            scaledViewportCells
            maybeJumpCursor
        | x <- [0 .. miniMapWidth - 1]
        ]

showMiniMapCellImage :: Int -> Int -> Set.Set Cell -> Set.Set Cell -> Maybe Cell -> Image
showMiniMapCellImage x y scaledCells scaledViewportCells maybeJumpCursor
  | Just (x, y) == maybeJumpCursor = string (cursorAttrFor alive) contents
  | (x, y) `Set.member` scaledViewportCells = string (viewportAttrFor alive) contents
  | otherwise = string (attrFor alive) contents
  where
    alive = (x, y) `Set.member` scaledCells
    contents = if alive then "#" else "."
    viewportAttrFor isAliveCell = if isAliveCell then liveViewportBgAttr else viewportBgAttr

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

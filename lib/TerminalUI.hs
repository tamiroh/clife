module TerminalUI
  ( animateGenerations
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import GameOfLife (Board, Cell, isAlive, nextGeneration)
import System.IO
  ( BufferMode (NoBuffering),
    hFlush,
    hGetBuffering,
    hGetEcho,
    hReady,
    hSetBuffering,
    hSetEcho,
    stdin,
    stdout
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
  originalBuffering <- hGetBuffering stdin
  originalEcho <- hGetEcho stdin
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  (hideCursor >> runLoop generationLimit delayInMicroseconds 0 (0, 0) (0, 0) board)
    `finally` do
      showCursor
      hSetEcho stdin originalEcho
      hSetBuffering stdin originalBuffering

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

data Direction
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight

readArrowKey :: IO (Maybe Direction)
readArrowKey = do
  maybeFirst <- readCharIfReady
  case maybeFirst of
    Just '\ESC' -> do
      maybeSecond <- readCharIfReady
      maybeThird <- readCharIfReady
      pure $
        case (maybeSecond, maybeThird) of
          (Just '[', Just 'A') -> Just MoveUp
          (Just '[', Just 'B') -> Just MoveDown
          (Just '[', Just 'C') -> Just MoveRight
          (Just '[', Just 'D') -> Just MoveLeft
          _ -> Nothing
    _ -> pure Nothing

readCharIfReady :: IO (Maybe Char)
readCharIfReady = do
  ready <- hReady stdin
  if ready
    then Just <$> getChar
    else pure Nothing

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

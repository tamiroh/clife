module TerminalControl
  ( Direction (..),
    Input (..),
    clearConsole,
    clearFromCursorDown,
    moveCursorHome,
    hideCursor,
    showCursor,
    withRawTerminalInput,
    readInput,
  )
where

import Control.Exception (finally)
import System.IO
  ( BufferMode (NoBuffering),
    hGetBuffering,
    hGetEcho,
    hReady,
    hSetBuffering,
    hSetEcho,
    stdin,
  )

data Direction
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight

data Input
  = MoveCursor Direction
  | ToggleCell
  | ToggleRunning
  | Quit

clearConsole :: IO ()
clearConsole = putStr "\ESC[2J\ESC[H"

clearFromCursorDown :: IO ()
clearFromCursorDown = putStr "\ESC[J"

moveCursorHome :: IO ()
moveCursorHome = putStr "\ESC[H"

hideCursor :: IO ()
hideCursor = putStr "\ESC[?25l"

showCursor :: IO ()
showCursor = putStr "\ESC[?25h"

withRawTerminalInput :: IO a -> IO a
withRawTerminalInput action = do
  originalBuffering <- hGetBuffering stdin
  originalEcho <- hGetEcho stdin
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  action `finally` do
    hSetEcho stdin originalEcho
    hSetBuffering stdin originalBuffering

readInput :: IO (Maybe Input)
readInput = do
  maybeFirst <- readCharIfReady
  case maybeFirst of
    Just 'q' -> pure (Just Quit)
    Just 'Q' -> pure (Just Quit)
    Just 'x' -> pure (Just ToggleCell)
    Just 'X' -> pure (Just ToggleCell)
    Just ' ' -> pure (Just ToggleRunning)
    Just '\ESC' -> do
      maybeSecond <- readCharIfReady
      maybeThird <- readCharIfReady
      pure $
        case (maybeSecond, maybeThird) of
          (Just '[', Just 'A') -> Just (MoveCursor MoveUp)
          (Just '[', Just 'B') -> Just (MoveCursor MoveDown)
          (Just '[', Just 'C') -> Just (MoveCursor MoveRight)
          (Just '[', Just 'D') -> Just (MoveCursor MoveLeft)
          _ -> Nothing
    _ -> pure Nothing

readCharIfReady :: IO (Maybe Char)
readCharIfReady = do
  ready <- hReady stdin
  if ready
    then Just <$> getChar
    else pure Nothing

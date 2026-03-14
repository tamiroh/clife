module TerminalControl
  ( Direction (..),
    clearConsole,
    moveCursorHome,
    hideCursor,
    showCursor,
    withRawTerminalInput,
    readArrowKey
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
    stdin
  )

data Direction
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight

clearConsole :: IO ()
clearConsole = putStr "\ESC[2J\ESC[H"

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

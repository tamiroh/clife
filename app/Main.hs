module Main where

import BoardFile (loadBoard)
import TerminalUI (animateGenerations)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      result <- loadBoard path
      case result of
        Right board -> animateGenerations Nothing 50000 board
        Left message -> putStrLn ("Failed to load board: " ++ message)
    _ -> putStrLn "Usage: cabal run clife -- <board-file.json>"

module Main where

import Board (Board)
import qualified BoardFile
import qualified RleFile
import System.Environment (getArgs)
import System.FilePath (takeExtension)
import TerminalUI (RunConfig (..), animateGenerations)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      result <- loadBoardFor path
      case result of
        Right board ->
          animateGenerations
            RunConfig {runGenerationLimit = Nothing, runFrameDelayMicroseconds = 50000}
            board
        Left message -> putStrLn ("Failed to load board: " ++ message)
    _ -> putStrLn "Usage: clife <board-file.json|board-file.rle>"

loadBoardFor :: FilePath -> IO (Either String Board)
loadBoardFor path
  | takeExtension path == ".rle" = RleFile.loadBoard path
  | otherwise = BoardFile.loadBoard path

module TerminalRender
  ( miniMapBounds,
    miniMapSizeFor,
    miniMapTargetCellFor,
    scaleCoordinate,
  )
where

import Board (Board, Cell, liveCells)
import qualified Data.Set as Set

maxMiniMapWidth :: Int
maxMiniMapWidth = 28

maxMiniMapHeight :: Int
maxMiniMapHeight = 14

miniMapBounds :: (Int, Int) -> [Cell] -> Cell -> (Cell, Cell)
miniMapBounds (viewportWidth, viewportHeight) boardCells (viewportX, viewportY) =
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

unscaleCoordinate :: Int -> Int -> Int -> Int -> Int
unscaleCoordinate value minValue spanValue targetSize
  | targetSize <= 1 = minValue
  | otherwise =
      minValue + (value * spanValue) `div` max 1 (targetSize - 1)

miniMapSizeFor :: (Int, Int) -> Board -> Cell -> (Int, Int)
miniMapSizeFor viewportSize board viewport
  | Set.null (liveCells board) = (1, 1)
  | otherwise = miniMapSize spanX spanY
  where
    boardCells = Set.toList (liveCells board)
    (_, (spanX, spanY)) = miniMapBounds viewportSize boardCells viewport

miniMapTargetCellFor :: (Int, Int) -> Board -> Cell -> Cell -> Cell
miniMapTargetCellFor viewportSize board viewport (miniMapX, miniMapY) =
  ( unscaleCoordinate miniMapX minX spanX miniMapWidth,
    unscaleCoordinate miniMapY minY spanY miniMapHeight
  )
  where
    boardCells = Set.toList (liveCells board)
    ((minX, minY), (spanX, spanY)) = miniMapBounds viewportSize boardCells viewport
    (miniMapWidth, miniMapHeight) = miniMapSizeFor viewportSize board viewport

miniMapSize :: Int -> Int -> (Int, Int)
miniMapSize spanX spanY
  | spanX * maxMiniMapHeight >= spanY * maxMiniMapWidth =
      (maxMiniMapWidth, max 1 (ceilingDiv (spanY * maxMiniMapWidth) spanX))
  | otherwise =
      (max 1 (ceilingDiv (spanX * maxMiniMapHeight) spanY), maxMiniMapHeight)

ceilingDiv :: Int -> Int -> Int
ceilingDiv numerator denominator =
  (numerator + denominator - 1) `div` denominator
